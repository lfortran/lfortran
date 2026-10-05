#!/usr/bin/env python3

import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parent.parent
INTEGRATION = ROOT / "integration_tests"


class SmokeSelectionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workspace = tempfile.TemporaryDirectory(prefix="lfortran-ci-tests-")
        cls.addClassCleanup(cls.workspace.cleanup)
        cls.directory = Path(cls.workspace.name).resolve()
        cls.bin = cls.directory / "bin"
        cls.bin.mkdir()
        # Configure-time backends must select tests before invoking the compiler.
        compiler = cls.bin / "lfortran"
        compiler.write_text(
            f"#!{sys.executable}\n"
            "import json, os, sys\n"
            "with open(os.environ['CI_COMPILER_LOG'], 'a') as log:\n"
            "    log.write(json.dumps(sys.argv[1:]) + '\\n')\n"
        )
        compiler.chmod(0o755)
        cls.configurations = {}

    def configure(self, backend, smoke=True, flags=()):
        key = (backend, smoke, flags)
        if key not in self.configurations:
            build = Path(tempfile.mkdtemp(prefix="build-", dir=self.directory))
            compiler_log = build.with_suffix(".jsonl")
            env = dict(os.environ)
            env["PATH"] = str(self.bin) + os.pathsep + env["PATH"]
            env["FC"] = str(self.bin / "lfortran")
            env["CI_COMPILER_LOG"] = str(compiler_log)
            command = [
                "cmake", "-S", str(INTEGRATION), "-B", str(build),
                f"-DCURRENT_BINARY_DIR={build}",
                f"-DLFORTRAN_BACKEND={backend}",
                f"-DLFORTRAN_SMOKE={'ON' if smoke else 'OFF'}",
                "-DCMAKE_Fortran_COMPILER_WORKS=ON",
                "-DCMAKE_Fortran_COMPILER_FORCED=ON",
                *flags,
            ]
            result = subprocess.run(command, cwd=build, env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            result = subprocess.run(
                ["ctest", "--show-only=json-v1"], cwd=build,
                capture_output=True, text=True,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            names = {test["name"] for test in json.loads(result.stdout)["tests"]}
            calls = [json.loads(line) for line in compiler_log.read_text().splitlines()]
            self.configurations[key] = (names, calls)
        return self.configurations[key]

    def test_manifest_names_are_registered_and_unique(self):
        source = (INTEGRATION / "CMakeLists.txt").read_text()
        registered = set(re.findall(r"(?:RUN|COMPILE)\(\s*NAME\s+([^\s)]+)", source))
        manifest = re.sub(r"#.*", "", (INTEGRATION / "smoke_tests.cmake").read_text())
        names = manifest.split("set(LFORTRAN_SMOKE_TESTS", 1)[1].rsplit(")", 1)[0].split()
        self.assertEqual(len(names), len(set(names)))
        self.assertFalse(set(names) - registered, set(names) - registered)

    def test_llvm_smoke_is_bounded_and_full_remains_full(self):
        smoke, _ = self.configure("llvm")
        full, _ = self.configure("llvm", smoke=False)
        self.assertGreaterEqual(len(smoke), 200)
        self.assertLessEqual(len(smoke), 300)
        self.assertGreater(len(full), 4000)
        self.assertLess(smoke, full)
        self.assertTrue({
            "program_cmake_01", "arrays_28", "derived_types_01",
            "finalization_01", "bindc_iso_fb_01", "read_94",
            "preprocessor_define_equals", "fixed_form_module_01",
        } <= smoke)

    def test_option_suffixes_do_not_change_selection(self):
        names, _ = self.configure("llvm", flags=(
            "-DFAST=ON", "-DSTD_F23=ON", "-DLLVM_GOC=ON", "-DDETECT_LEAKS=ON",
        ))
        self.assertIn("expr_02_STD_F23_LLVM_GOC_DETECT_LEAKS_FAST", names)
        self.assertGreaterEqual(len(names), 200)
        self.assertLessEqual(len(names), 300)

    def test_empty_selection_is_an_error(self):
        with self.assertRaisesRegex(AssertionError, "No smoke tests selected"):
            self.configure("missing_backend")

    def test_configure_time_compilation_is_filtered(self):
        for backend in ("wasm", "llvmImplicit"):
            with self.subTest(backend=backend):
                selected, calls = self.configure(backend)
                full, full_calls = self.configure(backend, smoke=False)
                compiled = [
                    args for args in calls
                    if any(arg.endswith(".f90") for arg in args)
                ]
                full_compiled = [
                    args for args in full_calls
                    if any(arg.endswith(".f90") for arg in args)
                ]
                self.assertEqual(len(compiled), len(selected))
                self.assertEqual(len(full_compiled), len(full))
                self.assertTrue(selected <= full)
                if backend == "wasm":
                    self.assertLess(len(compiled), len(full_compiled))

    def test_small_backend_coverage_is_nonempty(self):
        for backend in ("llvm2", "llvm_rtlib", "llvm_integer_8", "llvm_nopragma",
                        "llvm_submodule", "llvm_single_invocation", "mlir",
                        "llvm_wasm", "llvm_wasm_emcc"):
            with self.subTest(backend=backend):
                names, _ = self.configure(backend)
                self.assertTrue(names)


class RunnerTests(unittest.TestCase):
    def test_protected_status_requires_compatibility_on_prs(self):
        source = (ROOT / ".github/workflows/Quick-Checks-CI.yml").read_text()
        status = source.split("\n  quick_status:\n", 1)[1]
        self.assertIn("name: Build LFortran to WASM and Upload\n", status)
        self.assertIn("needs: [Build, compatibility, build_to_wasm_and_upload]\n", status)
        self.assertIn("if: ${{ !cancelled() }}", status)
        script = status.split("        run: |\n", 1)[1]
        cases = (
            ("pull_request", "false", "success", "success", "success", True),
            ("pull_request", "false", "success", "success", "failure", False),
            ("pull_request", "false", "success", "success", "skipped", False),
            ("pull_request", "false", "failure", "success", "success", False),
            ("pull_request", "false", "success", "failure", "success", False),
            ("pull_request", "true", "success", "success", "skipped", True),
            ("pull_request", "true", "failure", "success", "skipped", False),
            ("push", "false", "success", "success", "skipped", True),
            ("workflow_dispatch", "true", "success", "success", "skipped", True),
            ("push", "false", "skipped", "success", "skipped", False),
        )
        for event, full, build, wasm, compatibility, success in cases:
            with self.subTest(event=event, full=full, build=build, wasm=wasm, compatibility=compatibility):
                env = dict(os.environ, EVENT_NAME=event, FULL_COVERAGE=full, BUILD_RESULT=build,
                           WASM_RESULT=wasm, COMPATIBILITY_RESULT=compatibility)
                result = subprocess.run(["bash"], input=script, env=env,
                                        capture_output=True, text=True)
                self.assertEqual(result.returncode == 0, success, result.stdout + result.stderr)

    def test_smoke_flag_reaches_cmake_before_build_and_ctest(self):
        spec = importlib.util.spec_from_file_location(
            "integration_runner", INTEGRATION / "run_tests.py"
        )
        runner = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(runner)
        for smoke in (False, True):
            with self.subTest(smoke=smoke), patch.object(runner, "run_cmd") as command:
                runner.run_test("llvm", "lf", smoke=smoke)
                commands = [call.args[0] for call in command.call_args_list]
                configure = next(cmd for cmd in commands if " cmake " in cmd)
                self.assertEqual("-DLFORTRAN_SMOKE=ON" in configure, smoke)
                self.assertEqual("--no-tests=error" in commands[-1], smoke)
                self.assertLess(commands.index(configure), commands.index("make -j8"))

    def test_application_catalog_has_no_quick_subset(self):
        source = (ROOT / "ci/test_third_party_codes.sh").read_text()
        self.assertNotIn("--quick", source)
        definitions = source.split("while [[ $# -gt 0 ]]; do", 1)[0]
        sections = re.findall(r'^time_section "([^"]+)"', source, re.MULTILINE)
        script = definitions + "\nset +x\n"
        for section in sections:
            script += f'time_section "{section}" ":"\n'
        result = subprocess.run(["bash"], input=script, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        selected = [
            line[len("##[group] "):] for line in result.stdout.splitlines()
            if line.startswith("##[group] ") and line != "##[group] Setup"
        ]
        self.assertEqual(selected, sections)


class WorkflowPolicyTests(unittest.TestCase):
    def test_application_catalog_runs_only_on_main_pushes(self):
        workflows = ROOT / ".github/workflows"
        callers = [path.name for path in workflows.glob("*.yml")
                   if "ci/test_third_party_codes.sh" in path.read_text()]
        self.assertEqual(callers, ["Compiler-Compatibility-CI.yml"])
        source = (workflows / callers[0]).read_text()
        step = source.split("      - name: Test third party codes\n", 1)[1].split("\n      - ", 1)[0]
        condition = re.search(r"^\s*if: (.+)$", step, re.MULTILINE).group(1)
        self.assertEqual(condition,
            "${{ github.event_name == 'push' && github.ref == 'refs/heads/main' "
            "&& !inputs.quick && (matrix.llvm-version == '11' || matrix.llvm-version == '19' "
            "|| contains(matrix.os, 'macos')) }}")
        self.assertNotIn("--quick", step)
        self.assertIn('LAPACK_MODE="full"', step)

    def test_capability_checks_remain_in_quick(self):
        workflows = ROOT / ".github/workflows"
        quick = (workflows / "Quick-Checks-CI.yml").read_text()
        shared = (workflows / "Compiler-Compatibility-CI.yml").read_text()
        self.assertIn("./run_tests.py -b metal -j3", quick)
        self.assertIn("./run_tests.py -b cuda_cpu -j3", quick)
        self.assertIn("quick: true", quick)
        caffeine = shared.split("      - name: Test coarray runtime with Caffeine\n", 1)[1]
        caffeine = caffeine.split("\n  test_llvm_wasm:", 1)[0]
        self.assertIn("if: ${{ !inputs.quick || matrix.llvm-version == '11' }}", caffeine)
        self.assertIn("run: ci/test_caffeine.sh --verify-all-passes", caffeine)
        self.assertNotIn("github.event_name", caffeine)

    def test_full_quick_does_not_cancel_or_duplicate_normal_quick(self):
        source = (ROOT / ".github/workflows/Quick-Checks-CI.yml").read_text()
        self.assertIn("group: quick-${{ inputs.full && 'full' || 'default' }}-", source)
        self.assertIn("if: github.event_name == 'pull_request' && !inputs.full", source)
        self.assertIn("FULL_COVERAGE: ${{ inputs.full }}", source)
        expression = re.search(r"^\s*LFORTRAN_TEST_SUITE: (.+)$", source, re.MULTILINE).group(1)
        self.assertIn("github.event_name == 'pull_request' && !inputs.full", expression)
        self.assertNotIn("LFORTRAN_CI_RELEASE", source)

    def test_exhaustive_gate_reads_current_labels(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-CI.yml").read_text()
        self.assertIn("types: [opened, reopened, synchronize]", source)
        gate = source.split("\n  gate:\n", 1)[1].split("\n  full_quick:\n", 1)[0]
        script = gate.split("        run: |\n", 1)[1]
        with tempfile.TemporaryDirectory(prefix="lfortran-ci-gate-") as temporary:
            directory = Path(temporary)
            gh = directory / "gh"
            gh.write_text(
                "#!/bin/sh\n"
                'if [ "$LABEL_RESULT" = error ]; then echo "API failed" >&2; exit 1; fi\n'
                'printf "%s\\n" "$LABEL_RESULT"\n'
            )
            gh.chmod(0o755)
            output = directory / "output"
            cases = (
                ("pull_request", "true", "run=true\n"),
                ("pull_request", "false", "run=false\n"),
                ("pull_request", "error", None),
                ("pull_request", "invalid", None),
                ("push", "error", "run=true\n"),
                ("workflow_dispatch", "error", "run=true\n"),
                ("pull_request_target", "true", None),
            )
            for event, label, expected in cases:
                with self.subTest(event=event, label=label):
                    output.write_text("")
                    env = dict(os.environ, PATH=str(directory) + os.pathsep + os.environ["PATH"],
                               GITHUB_EVENT_NAME=event, GITHUB_OUTPUT=str(output),
                               LABEL_RESULT=label, PR="123")
                    result = subprocess.run(
                        ["bash", "-e", "-o", "pipefail"], input=script, env=env,
                        capture_output=True, text=True,
                    )
                    self.assertEqual(result.returncode == 0, expected is not None)
                    self.assertEqual(output.read_text(), expected or "")

        jobs = source.split("\njobs:\n", 1)[1]
        blocks = re.split(r"(?m)^  ([\w-]+):\n", jobs)
        for index in range(1, len(blocks), 2):
            name, body = blocks[index:index + 2]
            if name in ("gate", "deploy_jupyterlite"):
                continue
            self.assertIn("needs: gate", body, name)
            self.assertIn("needs.gate.outputs.run == 'true'", body, name)
        self.assertIn("full: true", source)
        self.assertIn("quick: false", source)

    def test_label_controller_never_executes_pr_code(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-Label-CI.yml").read_text()
        self.assertIn("pull_request_target:", source)
        self.assertIn("if: github.event.label.name == 'Tests::Run-Exhaustive'", source)
        self.assertNotIn("actions/checkout", source)
        self.assertNotIn("workflow_dispatch", source)
        self.assertIn('gh run rerun "$run_id"', source)
        self.assertIn('if [ "$(head_sha)" != "$sha" ]; then', source)


class QuickScriptTests(unittest.TestCase):
    def setUp(self):
        workspace = tempfile.TemporaryDirectory(prefix="lfortran-ci-routing-")
        self.addCleanup(workspace.cleanup)
        self.directory = Path(workspace.name).resolve()
        self.calls = self.directory / "calls.jsonl"
        self.bin = self.directory / "bin"
        self.bin.mkdir()
        self.env = dict(os.environ, PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
                        CI_CALLS=str(self.calls))
        for name in (
            "src/bin/lfortran", "run_tests.py", "integration_tests/run_tests.py",
            "expr2", "modules_15", "intrinsics_04", "intrinsics_04s",
            "bin/gcc", "bin/clang", "bin/cl", "bin/nproc", "bin/cmake",
            "bin/make", "bin/ctest", "bin/pip",
        ):
            target = self.directory / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(
                f"#!{sys.executable}\n"
                "import json, os, sys\n"
                "from pathlib import Path\n"
                "with open(os.environ['CI_CALLS'], 'a') as log:\n"
                "    log.write(json.dumps([str(Path(sys.argv[0]).resolve()), sys.argv[1:]]) + '\\n')\n"
                "if Path(sys.argv[0]).name == 'nproc': print(3)\n"
            )
            target.chmod(0o755)

    def run_shell(self, script):
        result = subprocess.run(
            ["shell", "--norc", str(script)], cwd=self.directory, env=self.env,
            capture_output=True, text=True,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def test_platform_profiles(self):
        cases = (
            ("linux-default", "0", "0", "11", None, True, 4),
            ("linux-full", "0", "0", "11", "full", True, 4),
            ("mac-full", "0", "1", "11", "full", True, 2),
            ("mac-smoke", "0", "1", "11", "smoke", False, 2),
            ("recent-llvm", "0", "0", "21", "smoke", False, 4),
            ("windows", "1", "0", "11", "smoke", False, 0),
        )
        for name, win, mac, llvm, suite, reference, count in cases:
            with self.subTest(profile=name):
                work = self.directory / name
                work.mkdir()
                # ci/test.sh uses fixed relative paths and creates one build dir.
                for entry in ("src", "integration_tests", "run_tests.py", "expr2",
                              "modules_15", "intrinsics_04", "intrinsics_04s"):
                    if entry == "integration_tests":
                        (work / entry).mkdir()
                        (work / entry / "run_tests.py").symlink_to(
                            self.directory / entry / "run_tests.py"
                        )
                    else:
                        (work / entry).symlink_to(self.directory / entry)
                self.env.update(WIN=win, MACOS=mac, LFORTRAN_LLVM_VERSION=llvm)
                self.env.pop("LFORTRAN_TEST_SUITE", None)
                if suite is not None:
                    self.env["LFORTRAN_TEST_SUITE"] = suite
                self.calls.write_text("")
                result = subprocess.run(
                    ["shell", "--norc", str(ROOT / "ci/test.sh")], cwd=work, env=self.env,
                    capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                calls = [json.loads(line) for line in self.calls.read_text().splitlines()]
                reference_calls = [args for path, args in calls
                                   if path == str(self.directory / "run_tests.py")]
                integration_calls = [args for path, args in calls
                                     if path == str(self.directory / "integration_tests/run_tests.py")]
                self.assertEqual(bool(reference_calls), reference)
                self.assertEqual(len(integration_calls), count)
                for args in integration_calls:
                    self.assertNotIn("", args)
                    self.assertEqual("--smoke" in args, suite == "smoke")

    def test_build_types_preserve_existing_platform_configuration(self):
        source = (ROOT / "ci/build.sh").read_text()
        block = source.split('if [[ $WIN == "1" ]]; then # Windows', 1)[1]
        block = 'if [[ $WIN == "1" ]]; then # Windows' + block.split("\ncmake ", 1)[0]
        script = self.directory / "build-type.sh"
        script.write_text(block + '\necho "BUILD_TYPE=$BUILD_TYPE"\n')
        for win, expected in (("0", "Debug"), ("1", "Release")):
            for suite in ("smoke", "full"):
                with self.subTest(win=win, suite=suite):
                    self.env.update(WIN=win, LFORTRAN_TEST_SUITE=suite)
                    result = self.run_shell(script)
                    self.assertIn(f"BUILD_TYPE={expected}", result.stdout)


if __name__ == "__main__":
    unittest.main()
