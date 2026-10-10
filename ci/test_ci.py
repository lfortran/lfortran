#!/usr/bin/env python3

import importlib.util
import json
import os
from pathlib import Path
import re
import shlex
import shutil
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
    def test_quick_has_no_status_aggregate(self):
        # Branch protection requires every Quick job directly. Repository
        # variables are not passed to PRs from forks, so a vars-gated
        # aggregate cannot be switched off for PRs.
        source = (ROOT / ".github/workflows/Quick-Checks-CI.yml").read_text()
        self.assertNotIn("quick_status", source)
        self.assertNotIn("name: Build LFortran to WASM and Upload", source)
        self.assertNotIn("vars.", source)

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
    def test_application_catalog_runs_in_every_exhaustive_run_except_tags(self):
        workflows = ROOT / ".github/workflows"
        callers = [path.name for path in workflows.glob("*.yml")
                   if "ci/test_third_party_codes.sh" in path.read_text()]
        self.assertEqual(callers, ["Compiler-Compatibility-CI.yml"])
        source = (workflows / callers[0]).read_text()
        step = source.split("      - name: Test third party codes\n", 1)[1].split("\n      - ", 1)[0]
        condition = re.search(r"^\s*if: (.+)$", step, re.MULTILINE).group(1)
        self.assertEqual(condition,
            "${{ inputs.scope == 'exhaustive' && !startsWith(github.ref, 'refs/tags/') "
            "&& (matrix.llvm-version == '11' || matrix.llvm-version == '19' "
            "|| contains(matrix.os, 'macos')) }}")
        self.assertNotIn("--quick", step)
        self.assertIn('LAPACK_MODE="full"', step)

    def test_capability_checks_remain_in_quick(self):
        workflows = ROOT / ".github/workflows"
        quick = (workflows / "Quick-Checks-CI.yml").read_text()
        shared = (workflows / "Compiler-Compatibility-CI.yml").read_text()
        self.assertIn("./run_tests.py -b metal -j3", quick)
        self.assertIn("./run_tests.py -b cuda_cpu -j3", quick)
        self.assertIn("scope: quick", quick)
        caffeine = shared.split("      - name: Test coarray runtime with Caffeine\n", 1)[1]
        caffeine = caffeine.split("\n  test_llvm_wasm:", 1)[0]
        self.assertIn("if: ${{ inputs.scope != 'quick' || matrix.llvm-version == '11' }}", caffeine)
        self.assertIn("run: ci/test_caffeine.sh --verify-all-passes", caffeine)
        self.assertNotIn("github.event_name", caffeine)
        self.assertIn("LFORTRAN_COARRAY_BASE: ${{ github.event.pull_request.base.sha || github.event.before }}", caffeine)
        self.assertIn("LFORTRAN_COARRAY_HEAD: ${{ github.sha }}", caffeine)
        self.assertIn("fetch-depth: 0", shared)

    def test_quick_compatibility_setup_does_not_preinstall_reference_only_mpi(self):
        source = (ROOT / ".github/workflows/Compiler-Compatibility-CI.yml").read_text()
        setups = source.split("      - uses: mamba-org/setup-micromamba@v2.0.2\n")[1:]
        quick_setups = [step for step in setups if
                        "if: matrix.llvm-version == '7'\n" in step or
                        "if: contains(matrix.llvm-version, '23')" in step or
                        "if: ${{! (" in step]
        self.assertEqual(len(quick_setups), 3)
        for index, setup in enumerate(quick_setups):
            packages = setup.split("          create-args: >-\n", 1)[1].split("\n\n", 1)[0]
            for scope in ("quick", "exhaustive"):
                with self.subTest(setup=index, scope=scope):
                    expression = r"\$\{\{ inputs.scope == 'exhaustive' && '(openmpi=[^']+)' \|\| '' \}\}"
                    rendered = re.sub(
                        expression, lambda match: match[1] if scope == "exhaustive" else "",
                        packages,
                    )
                    rendered = re.sub(r"\$\{\{ matrix\.[^}]+ \}\}", "11", rendered)
                    mpi = [package for package in shlex.split(rendered) if package.startswith("openmpi")]
                    pin = ("openmpi=5.0.6=hb85ec53_102", "openmpi=5.0.10",
                           "openmpi=5.0.6=hb85ec53_102")[index]
                    self.assertEqual(mpi, [pin] if scope == "exhaustive" else [])
        for path in ("ci/environment.yml", "ci/environment_linux_llvm.yml"):
            self.assertNotRegex((ROOT / path).read_text(), r"(?m)^\s*-\s*openmpi\b")

    def test_quick_coverage_is_event_independent_and_never_replayed(self):
        workflows = ROOT / ".github/workflows"
        quick = (workflows / "Quick-Checks-CI.yml").read_text()
        extra = (workflows / "Exhaustive-Checks-CI.yml").read_text()
        self.assertIn("cancel-in-progress: ${{ github.event_name == 'pull_request' }}", quick)
        self.assertIn("  workflow_dispatch:\n", quick)
        self.assertNotIn("inputs.full", quick)
        self.assertNotIn("LFORTRAN_CI_RELEASE", quick)
        expression = re.search(r"^\s*LFORTRAN_TEST_SUITE: (.+)$", quick, re.MULTILINE).group(1)
        self.assertEqual(expression, "smoke")
        event_conditions = re.findall(r"^\s*if: (.*github\.event_name.*)$", quick, re.MULTILINE)
        self.assertEqual(event_conditions, ["github.event_name == 'push'"])
        upload = quick.split("      - name: Upload to wasm_builds\n", 1)[1]
        self.assertIn("if: github.event_name == 'push'", upload)
        compatibility = quick.split("\n  compatibility:\n", 1)[1].split("\n  test_llvm_wasm:\n", 1)[0]
        self.assertNotIn("if:", compatibility)
        self.assertIn("scope: quick", compatibility)
        self.assertNotIn("full_quick:", extra)
        self.assertNotIn("\n  quick:\n", extra)
        self.assertNotIn("Quick-Checks-CI.yml", extra)
        self.assertIn("name: Extended compiler checks", extra)
        self.assertNotIn("name: Compiler compatibility\n", extra)

    def test_coverage_matrix_has_quick_and_exhaustive_roles(self):
        source = (ROOT / ".github/workflows/Compiler-Compatibility-CI.yml").read_text()
        matrix = re.search(r"llvm-version: \$\{\{ fromJSON\('([^']+)'\)\[inputs.scope\]", source)
        self.assertIsNotNone(matrix)
        roles = json.loads(matrix.group(1))
        self.assertEqual(roles, {
            "quick": ["7", "11", "23"],
            "exhaustive": ["7", "8", "10", "11", "15", "17", "18", "19", "21", "22", "23"],
        })
        self.assertIn('"os":"macos-latest","llvm-version":"22"', source)
        self.assertIn("inputs.scope == 'quick' && fromJSON('[]')", source)

    def test_backend_jobs_belong_only_to_quick_with_stable_names(self):
        workflows = ROOT / ".github/workflows"
        quick = (workflows / "Quick-Checks-CI.yml").read_text()
        shared = (workflows / "Compiler-Compatibility-CI.yml").read_text()
        jobs = (
            ("test_llvm_wasm", "Test LLVM 19 WASM (ubuntu-latest)"),
            ("test_without_llvm", "Test without LLVM Backend"),
            ("test_mlir", "Test MLIR backend"),
        )
        for job, name in jobs:
            self.assertNotIn(f"\n  {job}:\n", shared)
            body = quick.split(f"\n  {job}:\n", 1)[1]
            body = re.split(r"\n  [\w-]+:\n", body, maxsplit=1)[0]
            self.assertIn(f"name: Compiler compatibility / {name}\n", body)
            self.assertNotIn("\n    if:", body)
        wasm = quick.split("\n  test_llvm_wasm:\n", 1)[1].split("\n  test_without_llvm:", 1)[0]
        self.assertIn("./run_tests.py -b llvm_wasm llvm_wasm_emcc\n", wasm)
        self.assertNotIn("--smoke", wasm)

    def test_full_platform_coverage_moves_to_exhaustive_without_changing_builds(self):
        workflows = ROOT / ".github/workflows"
        quick = (workflows / "Quick-Checks-CI.yml").read_text()
        extra = (workflows / "Exhaustive-Checks-CI.yml").read_text()
        quick_build = quick.split("\n  Build:\n", 1)[1].split("\n  compatibility:\n", 1)[0]
        platform = extra.split("\n  platform:\n", 1)[1].split("\n  debug_outOfSource:\n", 1)[0]
        self.assertEqual(
            re.findall(r'- os: ([\w-]+)\n\s+llvm-version: "(\d+)"', platform),
            [("macos-latest", "11"), ("ubuntu-latest", "11"), ("ubuntu-latest", "21")],
        )
        deployment_target = re.search(
            r"^  MACOSX_DEPLOYMENT_TARGET: (.+)$", quick, re.MULTILINE
        ).group(1)
        self.assertIn(f"MACOSX_DEPLOYMENT_TARGET: {deployment_target}", platform)
        for job in (quick_build, platform):
            self.assertIn("uses: ./.github/actions/build-platform", job)
            self.assertIn("os: ${{ matrix.os }}", job)
            self.assertIn("llvm-version: ${{ matrix.llvm-version }}", job)
        action = (ROOT / ".github/actions/build-platform/action.yml").read_text()
        self.assertIn("environment-file: ci/environment.yml", action)
        self.assertIn("ENABLE_RUNTIME_STACKTRACE=yes", action)
        self.assertIn("CXXFLAGS=\"-Werror -D_GLIBCXX_ASSERTIONS", action)
        self.assertIn("shell ci/build.sh", action)
        self.assertIn("shell ci\\build.sh", action)
        self.assertIn("bash ci/test_llvm_integration.sh --platform", platform)
        self.assertNotIn("shell ci/test.sh", platform)
        self.assertNotIn("github.event_name", platform)

    def test_quick_owns_full_cpu_regressions(self):
        source = (ROOT / ".github/workflows/Compiler-Compatibility-CI.yml").read_text()
        reference = source.split("      - name: Test Debug reference coverage\n", 1)[1]
        reference = reference.split("      - name: Test LLVM integration tests\n", 1)[0]
        self.assertIn("inputs.scope == 'quick' && matrix.llvm-version == '11'", reference)
        self.assertIn("./run_tests.py\n", reference)
        integration = source.split("      - name: Test LLVM integration tests\n", 1)[1]
        integration = integration.split("      - name: Test coarray", 1)[0]
        self.assertIn('[[ "$LFORTRAN_CI_SCOPE" == "quick" && "$LFORTRAN_LLVM_VERSION" == "11" ]]', integration)
        self.assertIn("bash ci/test_llvm_integration.sh --core", integration)
        self.assertNotIn("shell ci/test.sh", integration)
        quick = (ROOT / ".github/workflows/Quick-Checks-CI.yml").read_text()
        options = quick.split("      - name: Test full descriptor modes\n", 1)[1]
        options = options.split("\n      - ", 1)[0]
        self.assertIn("if: matrix.os == 'ubuntu-latest' && matrix.llvm-version == '21'", options)
        self.assertIn("bash ci/test_llvm_integration.sh --options", options)

    def test_required_checks_have_stable_names(self):
        source = (ROOT / ".github/workflows/Quick-Checks-CI.yml").read_text()
        wasm = source.split("\n  build_to_wasm_and_upload:\n", 1)[1]
        self.assertIn("name: Build LFortran to WASM\n", wasm)
        platform = source.split("\n  Build:\n", 1)[1].split("\n  compatibility:\n", 1)[0]
        self.assertIn("name: LFortran CI (OS=${{ matrix.os }}, LLVM=${{ matrix.llvm-version }})", platform)
        platforms = re.findall(r'- os: ([\w-]+)\n\s+llvm-version: "(\d+)"', platform)
        self.assertEqual(platforms, [
            ("macos-latest", "11"), ("ubuntu-latest", "11"),
            ("ubuntu-latest", "21"), ("windows-2025", "11"),
        ])
        documentation = (ROOT / "doc/src/installation.md").read_text()
        required = documentation.split("```text\n", 1)[1].split("```", 1)[0].splitlines()
        self.assertEqual(required, [
            f"LFortran CI (OS={platform}, LLVM={llvm})" for platform, llvm in platforms
        ] + [
            "Build LFortran to WASM",
            "Compiler compatibility / Test LLVM 7 (ubuntu-latest)",
            "Compiler compatibility / Test LLVM 11 (ubuntu-latest)",
            "Compiler compatibility / Test LLVM 23 (ubuntu-latest)",
            "Compiler compatibility / Test LLVM 19 WASM (ubuntu-latest)",
            "Compiler compatibility / Test without LLVM Backend",
            "Compiler compatibility / Test MLIR backend",
        ])

    def test_exhaustive_gate_reads_current_labels(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-CI.yml").read_text()
        self.assertIn("types: [opened, reopened, synchronize]", source)
        gate = source.split("\n  gate:\n", 1)[1].split("\n  compatibility:\n", 1)[0]
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
        self.assertIn("scope: exhaustive", source)

    def test_main_runs_are_coalesced_but_never_cancelled(self):
        workflows = ROOT / ".github/workflows"
        for name, prefix in (("Quick-Checks-CI.yml", "quick-"),
                             ("Exhaustive-Checks-CI.yml", "${{ github.workflow }}-")):
            with self.subTest(workflow=name):
                source = (workflows / name).read_text()
                block = source.split("\nconcurrency:\n", 1)[1].split("\n\n", 1)[0]
                group = re.search(r"^\s*group: (.+)$", block, re.MULTILINE).group(1)
                cancel = re.search(r"^\s*cancel-in-progress: \$\{\{ (.+) \}\}$",
                                   block, re.MULTILINE).group(1)
                self.assertTrue(group.startswith(prefix + "${{ "), group)
                self.assertTrue(group.endswith(" }}"), group)
                group = group[len(prefix) + 4:-3]
                self.assertEqual(cancel, "github.event_name == 'pull_request'")

                def evaluate(expression, event, ref, number=None):
                    context = {"github.event_name": event, "github.ref": ref,
                               "github.event.number": number, "github.sha": "sha-" + ref}
                    python = re.sub(r"github(\.\w+)+",
                                    lambda m: repr(context[m.group(0)]), expression)
                    return eval(python.replace("&&", " and ").replace("||", " or "))

                def key(event, ref, number=None):
                    return (evaluate(group, event, ref, number),
                            evaluate(cancel, event, ref, number))

                # Main pushes share one group and never cancel a running run.
                self.assertEqual(key("push", "refs/heads/main"), ("main", False))
                self.assertEqual(key("pull_request", "refs/pull/7/merge", 7), (7, True))
                self.assertEqual(key("push", "refs/tags/v1.0.0"),
                                 ("sha-refs/tags/v1.0.0", False))
                self.assertEqual(key("workflow_dispatch", "refs/heads/main"),
                                 ("sha-refs/heads/main", False))

    def test_exhaustive_coverage_is_event_independent(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-CI.yml").read_text()
        jobs = source.split("\njobs:\n", 1)[1].split("\n  compatibility:\n", 1)[1]
        self.assertNotIn("--smoke", jobs)
        self.assertNotIn("GITHUB_EVENT_NAME", jobs)
        publishing = {
            "if: ${{ github.event_name == 'push' && startsWith(github.ref, 'refs/tags/v') }}",
            "if: github.event_name == 'push'",
            "if: ${{ github.event_name == 'push' }}",
            "if: github.event_name == 'push' && github.ref == 'refs/heads/main'",
        }
        conditions = re.findall(r"^\s*(if: .*github\.event_name.*)$", jobs, re.MULTILINE)
        self.assertEqual(set(conditions), publishing)

    def test_label_controller_never_executes_pr_code(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-Label-CI.yml").read_text()
        self.assertIn("pull_request_target:", source)
        self.assertIn("if: github.event.label.name == 'Tests::Run-Exhaustive'", source)
        self.assertNotIn("actions/checkout", source)
        self.assertNotIn("workflow_dispatch", source)
        self.assertIn('gh run rerun "$run_id"', source)
        self.assertIn('if [ "$(head_sha)" != "$sha" ]; then', source)


    def test_cache_cleanup_deletes_fork_pr_caches_without_running_pr_code(self):
        source = (ROOT / ".github/workflows/Clean-Cache-CI.yml").read_text()
        triggers = source.split("\non:\n", 1)[1].split("\npermissions:\n", 1)[0]
        self.assertIn("pull_request_target:", triggers)
        self.assertNotIn("  pull_request:", triggers)
        self.assertIn("permissions:\n  actions: write\n", source)
        self.assertNotIn("actions/checkout", source)
        self.assertNotIn("gh extension", source)
        self.assertIn("REF: refs/pull/${{ github.event.pull_request.number }}/merge", source)
        self.assertIn('gh cache delete --all --ref "$REF" --succeed-on-no-caches', source)

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
            "build0.sh", "src/bin/lfortran", "run_tests.py", "integration_tests/run_tests.py",
            "expr2", "expr2-debug", "modules_15", "intrinsics_04", "intrinsics_04s",
            "bin/gcc", "bin/clang", "bin/cl", "bin/nproc", "bin/cmake",
            "bin/make", "bin/ctest", "bin/pip", "bin/llvm-dwarfdump", "bin/dsymutil",
        ):
            target = self.directory / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(
                f"#!{sys.executable}\n"
                "import json, os, sys\n"
                "from pathlib import Path\n"
                "with open(os.environ['CI_CALLS'], 'a') as log:\n"
                "    log.write(json.dumps([str(Path(sys.argv[0]).resolve()), sys.argv[1:]]) + '\\n')\n"
                "if (os.environ.get('CI_FAIL_TOOL') == Path(sys.argv[0]).name or\n"
                "    (os.environ.get('CI_FAIL_TOOL') == 'debug-link' and '-g' in sys.argv)):\n"
                "    sys.exit(int(os.environ.get('CI_FAIL_STATUS', '42')))\n"
                "if Path(sys.argv[0]).name in ('expr2', 'expr2-debug'):\n"
                "    print(os.environ.get('CI_EXPR_RESULT', '25'))\n"
                "if Path(sys.argv[0]).name == 'cmake' and 'CI_BUILD_ENV_LOG' in os.environ:\n"
                "    with open(os.environ['CI_BUILD_ENV_LOG'], 'a') as log:\n"
                "        log.write(json.dumps({key: os.environ.get(key, '') "
                "for key in ('CFLAGS', 'CXXFLAGS')}) + '\\n')\n"
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
            ("linux-smoke", "0", "0", "11", "smoke", True, 4),
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

    def test_balanced_quick_jobs_run_every_cpu_mode_without_smoke(self):
        quick_calls = []
        for suite, mode in (("full", ""), ("smoke", ""), ("smoke", "--core"), ("smoke", "--options")):
            with self.subTest(suite=suite, mode=mode):
                self.calls.write_text("")
                self.env.update(LFORTRAN_TEST_SUITE=suite, LFORTRAN_LLVM_VERSION="11", NPROC="3")
                command = ["bash", str(ROOT / "ci/test_llvm_integration.sh")]
                if mode:
                    command.append(mode)
                result = subprocess.run(command, cwd=self.directory, env=self.env,
                                        capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                calls = [json.loads(line)[1] for line in self.calls.read_text().splitlines()]
                self.assertEqual(len(calls), 3 if mode == "--options" else 7)
                for args in calls:
                    self.assertEqual("--smoke" in args, suite == "smoke" and not mode)
                if mode:
                    quick_calls.extend(calls)
        expected = (
            ["-b", "llvm", "-sc", "-j3"],
            ["-b", "llvm_submodule", "-sc", "-j3"],
            ["-b", "llvm", "--detect-leaks", "-j3"],
            ["-b", "llvm", "--std=f23", "-j3"],
            ["-b", "llvm", "-f", "--std=f23", "-j3"],
            ["-b", "llvm_single_invocation", "-j3"],
            ["-b", "llvm", "llvm2", "llvm_rtlib", "llvm_nopragma",
             "llvm_integer_8", "llvmImplicit", "-j3"],
            ["-b", "llvm", "llvmImplicit", "-f", "-j3"],
        )
        for args in expected:
            self.assertEqual(quick_calls.count(args), 1)
        self.assertEqual(len(quick_calls), 10)

    def test_invalid_core_arguments_fail_before_running_tests(self):
        self.env.update(NPROC="3", LFORTRAN_LLVM_VERSION="11")
        for arguments in (["--unknown"], ["--core", "--unknown"], ["--core", "--options"],
                          ["--platform", "--core"]):
            with self.subTest(arguments=arguments):
                result = subprocess.run(
                    ["bash", str(ROOT / "ci/test_llvm_integration.sh"), *arguments],
                    cwd=self.directory, env=self.env, capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 2)
                self.assertIn("usage:", result.stderr)
                self.assertFalse(self.calls.exists())

    def test_supplemental_platform_suites_keep_full_coverage(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-CI.yml").read_text()
        platform = source.split("\n  platform:\n", 1)[1].split("\n  debug_outOfSource:\n", 1)[0]
        step = platform.split("      - name: Test full platform coverage\n", 1)[1]
        script = step.split("        run: |\n", 1)[1]
        (self.directory / "ci").mkdir()
        (self.directory / "ci/test_llvm_integration.sh").symlink_to(
            ROOT / "ci/test_llvm_integration.sh"
        )
        normal = ["-b", "llvm", "llvm2", "llvm_rtlib", "llvm_nopragma",
                  "llvm_integer_8", "llvmImplicit", "-j3"]
        submodule = ["-b", "llvm_submodule", "-j3"]
        fast_variants = ["-b", "llvm2", "llvm_rtlib", "llvm_nopragma", "llvm_integer_8", "-f", "-j3"]
        fast = ["-b", "llvm", "llvmImplicit", "-f", "-j3"]
        for event in ("push", "pull_request", "workflow_dispatch"):
            for os_name, llvm in (("macos-latest", "11"), ("ubuntu-latest", "11"),
                                  ("ubuntu-latest", "21")):
                with self.subTest(event=event, platform=os_name, llvm=llvm):
                    self.calls.write_text("")
                    rendered = script.replace("${{ matrix.os }}", os_name)
                    rendered = rendered.replace("${{ matrix.llvm-version }}", llvm)
                    env = dict(self.env, GITHUB_EVENT_NAME=event, NPROC="3", LFORTRAN_TEST_SUITE="smoke")
                    result = subprocess.run(
                        ["bash", "-e", "-o", "pipefail"], input=rendered,
                        cwd=self.directory, env=env, capture_output=True, text=True,
                    )
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    calls = [json.loads(line) for line in self.calls.read_text().splitlines()]
                    reference = [args for path, args in calls
                                 if path == str(self.directory / "run_tests.py")]
                    integration = [args for path, args in calls
                                   if path == str(self.directory / "integration_tests/run_tests.py")]
                    macos = os_name == "macos-latest"
                    self.assertEqual(reference, [[]] if macos else [])
                    self.assertEqual(integration,
                                     [normal, submodule] if macos else
                                     [normal, fast_variants, fast, submodule])

    def test_installation_variants_run_full_suites_on_every_event(self):
        source = (ROOT / ".github/workflows/Exhaustive-Checks-CI.yml").read_text()
        blocks = re.split(r"(?m)^  ([\w-]+):\n", source.split("\njobs:\n", 1)[1])
        jobs = dict(zip(blocks[1::2], blocks[2::2]))
        (self.directory / "src").mkdir(exist_ok=True)
        (self.directory / "src/integration_tests").symlink_to(self.directory / "integration_tests")
        steps = (
            ("debug_outOfSource", "Test option combinations", 3),
            ("release", "Test Linux", 6),
        )
        for job, step, count in steps:
            body = jobs[job].split(f"      - name: {step}\n", 1)[1]
            body = body.split("\n      - ", 1)[0]
            script = body.split("        run: |\n", 1)[1]
            for event in ("push", "pull_request", "workflow_dispatch"):
                with self.subTest(job=job, event=event):
                    self.calls.write_text("")
                    env = dict(self.env, GITHUB_EVENT_NAME=event)
                    result = subprocess.run(["bash", "-e", "-o", "pipefail"],
                        input=script, cwd=self.directory, env=env,
                        capture_output=True, text=True)
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    calls = [json.loads(line) for line in self.calls.read_text().splitlines()]
                    integration = [args for path, args in calls
                                   if path == str(self.directory / "integration_tests/run_tests.py")]
                    reference = [args for path, args in calls
                                 if path == str(self.directory / "run_tests.py")]
                    self.assertEqual(len(integration), count)
                    self.assertEqual(len(reference), 2 if job == "release" else 0)
                    for args in integration:
                        self.assertNotIn("--smoke", args)

    def test_compatibility_build_configuration(self):
        source = (ROOT / ".github/workflows/Compiler-Compatibility-CI.yml").read_text()
        build = source.split("      - name: Build\n", 1)[1].split("\n      - ", 1)[0]
        script = build.split("        run: |\n", 1)[1]
        environment_log = self.directory / "build-environment.jsonl"
        self.env.update(CI_BUILD_ENV_LOG=str(environment_log), CFLAGS="", CXXFLAGS="")
        cases = (
            ("quick", "ubuntu-latest", "7", True),
            ("quick", "ubuntu-latest", "11", True),
            ("quick", "ubuntu-latest", "23", True),
            ("exhaustive", "ubuntu-latest", "7", True),
            ("exhaustive", "ubuntu-latest", "11", True),
            ("exhaustive", "ubuntu-latest", "19", True),
            ("exhaustive", "ubuntu-latest", "21", True),
            ("exhaustive", "ubuntu-latest", "22", True),
            ("exhaustive", "ubuntu-latest", "23", True),
            ("exhaustive", "macos-latest", "22", True),
        )
        for scope, platform, llvm, enabled in cases:
            with self.subTest(scope=scope, platform=platform, llvm=llvm):
                self.calls.write_text("")
                environment_log.write_text("")
                rendered = script.replace("${{ matrix.os }}", platform)
                rendered = rendered.replace("${{ matrix.llvm-version }}", llvm)
                env = dict(self.env, LFORTRAN_CI_SCOPE=scope, CONDA_PREFIX=str(self.directory))
                result = subprocess.run(
                    ["bash", "-e", "-o", "pipefail"], input=rendered,
                    cwd=self.directory, env=env, capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                calls = [json.loads(line) for line in self.calls.read_text().splitlines()]
                cmake = [args for path, args in calls if path == str(self.bin / "cmake")]
                self.assertEqual(len(cmake), 2)
                self.assertEqual("-DWITH_RUNTIME_STACKTRACE=yes" in cmake[0], enabled)
                self.assertEqual([arg for arg in cmake[0] if arg.startswith("-DWITH_RUNTIME_STACKTRACE=")],
                                 ["-DWITH_RUNTIME_STACKTRACE=yes"])
                debug = scope == "quick" and llvm == "11"
                self.assertIn(f"-DCMAKE_BUILD_TYPE={'Debug' if debug else 'Release'}", cmake[0])
                self.assertEqual("-DWITH_INTERNAL_ALLOC_CHECK=yes" in cmake[0], debug)
                flags = json.loads(environment_log.read_text().splitlines()[0])
                expected = ("-Werror -D_GLIBCXX_ASSERTIONS "
                            "-D_LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_DEBUG")
                self.assertEqual(flags, dict.fromkeys(("CFLAGS", "CXXFLAGS"), expected if debug else ""))

    def test_application_runtime_stacktrace_probes_and_failures(self):
        source = (ROOT / ".github/workflows/Compiler-Compatibility-CI.yml").read_text()
        step = source.split("      - name: Test application runtime stacktraces\n", 1)[1]
        step = step.split("\n      - ", 1)[0]
        self.assertIn("inputs.scope == 'exhaustive'", step)
        self.assertIn("matrix.llvm-version == '11'", step)
        self.assertIn("matrix.llvm-version == '19'", step)
        self.assertIn("contains(matrix.os, 'macos')", step)
        self.assertNotIn("github.event_name", step)
        config = self.directory / "src/libasr/config.h"
        config.parent.mkdir()
        config.write_text("#define HAVE_RUNTIME_STACKTRACE\n")
        script = step.split("        run: |\n", 1)[1]
        for platform in ("ubuntu-latest", "macos-latest"):
            with self.subTest(platform=platform):
                self.calls.write_text("")
                rendered = script.replace("${{ matrix.os }}", platform)
                result = subprocess.run(["bash", "-e", "-o", "pipefail"], input=rendered,
                                        cwd=self.directory, env=self.env, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                calls = [(Path(path).name, args)
                         for path, args in map(json.loads, self.calls.read_text().splitlines())]
                expected = [("llvm-dwarfdump", ["--version"])]
                if platform == "macos-latest":
                    expected.append(("dsymutil", ["--version"]))
                expected += [
                    ("lfortran", ["-v", "examples/expr2.f90", "-o", "expr2"]),
                    ("expr2", []),
                    ("lfortran", ["-v", "-g", "examples/expr2.f90", "-o", "expr2-debug"]),
                    ("expr2-debug", []),
                ]
                self.assertEqual(calls, expected)
        for failure in ("llvm-dwarfdump", "dsymutil", "lfortran", "debug-link", "expr2", "expr2-debug"):
            with self.subTest(failure=failure):
                result = subprocess.run(
                    ["bash", "-e", "-o", "pipefail"], input=rendered, cwd=self.directory,
                    env=dict(self.env, CI_FAIL_TOOL=failure), capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 42, result.stdout + result.stderr)
        for tool in ("llvm-dwarfdump", "dsymutil"):
            with self.subTest(missing=tool):
                result = subprocess.run(
                    ["bash", "-e", "-o", "pipefail"], input=rendered, cwd=self.directory,
                    env=dict(self.env, CI_FAIL_TOOL=tool, CI_FAIL_STATUS="127"),
                    capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 127, result.stdout + result.stderr)
        result = subprocess.run(
            ["bash", "-e", "-o", "pipefail"], input=rendered, cwd=self.directory,
            env=dict(self.env, CI_EXPR_RESULT="24"), capture_output=True, text=True,
        )
        self.assertNotEqual(result.returncode, 0)
        config.write_text("/* runtime support is disabled */\n")
        self.calls.write_text("")
        result = subprocess.run(["bash", "-e", "-o", "pipefail"], input=rendered,
                                cwd=self.directory, env=self.env, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.calls.read_text(), "")

    def test_full_descriptor_owner_keeps_platform_checks(self):
        action = (ROOT / ".github/actions/build-platform/action.yml").read_text()
        step = action.split("    - name: Build (Linux / macOS)\n", 1)[1].split("\n    - name:", 1)[0]
        script = step.split("      run: |\n", 1)[1].split("        shell ci/build.sh", 1)[0]
        build = (ROOT / "ci/build.sh").read_text()
        block = build.split('if [[ $WIN == "1" ]]; then # Windows', 1)[1]
        block = 'if [[ $WIN == "1" ]]; then # Windows' + block.split("\ncmake --build", 1)[0]
        environment_log = self.directory / "build-environment.jsonl"
        env = dict(self.env, CI_BUILD_ENV_LOG=str(environment_log),
                   LFORTRAN_CMAKE_GENERATOR="Ninja", ENABLE_RUNTIME_STACKTRACE="yes")
        result = subprocess.run(
            ["bash", "-e"], input=script + "\n" + block,
            cwd=self.directory, env=env, capture_output=True, text=True,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        calls = [json.loads(line)[1] for line in self.calls.read_text().splitlines()]
        self.assertEqual(len(calls), 1)
        self.assertIn("-DWITH_INTERNAL_ALLOC_CHECK=yes", calls[0])
        self.assertIn("-DCMAKE_BUILD_TYPE=Debug", calls[0])
        expected = ("-Werror -D_GLIBCXX_ASSERTIONS "
                    "-D_LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_DEBUG")
        flags = json.loads(environment_log.read_text())
        self.assertEqual(flags, dict.fromkeys(("CFLAGS", "CXXFLAGS"), expected))

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


class CaffeineTests(unittest.TestCase):
    def setUp(self):
        workspace = tempfile.TemporaryDirectory(prefix="lfortran-ci-caffeine-")
        self.addCleanup(workspace.cleanup)
        self.directory = Path(workspace.name).resolve()
        self.bin = self.directory / "bin"
        self.bin.mkdir()
        self.calls = self.directory / "calls.jsonl"
        self.git = shutil.which("git")
        self.env = dict(os.environ, PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
                        HOME=str(self.directory), CONDA_PREFIX=str(self.directory),
                        CI_CALLS=str(self.calls), CI_REAL_GIT=self.git, CI_OS="Linux",
                        CI_FPM_PRESENT="true", CI_MPI_PRESENT="false", CAF_IMAGES="2")
        for key in ("LFORTRAN_CI_SCOPE", "LFORTRAN_COARRAY_BASE", "LFORTRAN_COARRAY_HEAD"):
            self.env.pop(key, None)
        (self.directory / "integration_tests").mkdir()
        self.manifest = self.directory / "integration_tests/CMakeLists.txt"
        self.manifest.write_text(
            "RUN(NAME coarrays_03 EXTRA_ARGS --coarray=true)\n"
            "RUN(NAME coarrays_51 EXTRA_ARGS --coarray=true --separate-compilation "
            "EXTRAFILES coarrays_51_m.f90 NUM_IMAGES=4)\n"
            "RUN(NAME coarrays_11 EXTRA_ARGS --coarray=true NUM_IMAGES=6)\n"
            "RUN(NAME unrelated LABELS llvm)\n"
        )
        for name in ("coarrays_03", "coarrays_51", "coarrays_51_m", "coarrays_11", "unrelated"):
            (self.directory / f"integration_tests/{name}.f90").write_text(
                f"program {name}\nend program\n"
            )
        (self.directory / "ci").mkdir()
        helper = ROOT / "ci/coarray_tests.py"
        if helper.exists():
            shutil.copyfile(helper, self.directory / "ci/coarray_tests.py")
        (self.directory / "ci/test_caffeine.sh").write_text("# harness input\n")
        (self.directory / "ci/environment_linux_llvm.yml").write_text("dependencies: []\n")
        self.git_run("init", "-q")
        self.git_run("config", "user.email", "ci-tests@example.invalid")
        self.git_run("config", "user.name", "CI Tests")
        self.base = self.commit("integration_tests", "ci")
        mock = (
            f"#!{sys.executable}\n"
            "import json, os, subprocess, sys\n"
            "from pathlib import Path\n"
            "name = Path(sys.argv[0]).name\n"
            "args = sys.argv[1:]\n"
            "key = name\n"
            "if name == 'run-fpm.sh': key = 'unit' if args[0] == 'test' else 'info'\n"
            "if name == 'lfortran' and args == ['--version']: key = 'version'\n"
            "with open(os.environ['CI_CALLS'], 'a') as log:\n"
            "    log.write(json.dumps({'command': name, 'args': args, 'key': key,\n"
            "        'env': {k: os.environ.get(k) for k in ('FC', 'CC', 'CXX', 'CAF_IMAGES')}}) + '\\n')\n"
            "if os.environ.get('CI_FAIL') == key: sys.exit(42)\n"
            "if name == 'git':\n"
            "    if os.environ.get('CI_GIT_FAIL') == args[0]: sys.exit(42)\n"
            "    if args[0] == 'clone':\n"
            "        dest = Path('caffeine' if 'caffeine' in args[-1] else 'OpenCoarrays')\n"
            "        dest.mkdir()\n"
            "        if dest.name == 'caffeine':\n"
            "            for tool in ('install.sh', 'run-fpm.sh'):\n"
            "                target = dest / tool\n"
            "                target.write_text(Path(__file__).read_text())\n"
            "                target.chmod(0o755)\n"
            "    elif args[0] != 'checkout':\n"
            "        os.execv(os.environ['CI_REAL_GIT'], [os.environ['CI_REAL_GIT'], *args])\n"
            "elif name == 'uname': print(os.environ['CI_OS'])\n"
            "elif name in ('fpm', 'mpifort'):\n"
            "    present = os.environ['CI_FPM_PRESENT' if name == 'fpm' else 'CI_MPI_PRESENT']\n"
            "    marker = Path(os.environ['HOME']) / (name + '-installed')\n"
            "    if present != 'true' and not marker.exists(): sys.exit(127)\n"
            "    if (name == 'mpifort' and args == ['--version'] and\n"
            "            os.environ.get('CI_MPI_BUILD_COMPILER_MISSING') == 'true'):\n"
            "        print('The Open MPI wrapper compiler was unable to find the specified compiler')\n"
            "        print('x86_64-conda-linux-gnu-gfortran in your PATH.')\n"
            "        sys.exit(1)\n"
            "    print('Version: 0.12.0' if name == 'fpm' else 'mpifort: Open MPI 5.0.6')\n"
            "elif name == 'micromamba':\n"
            "    tool = 'fpm' if any(a.startswith('fpm=') for a in args) else 'mpifort'\n"
            "    (Path(os.environ['HOME']) / (tool + '-installed')).touch()\n"
            "elif name == 'cafrun': print('Error: OSC UCX component priority\\n')\n"
            "elif name == 'sed': sys.stdout.write(sys.stdin.read())\n"
        )
        for name in ("git", "uname", "micromamba", "mpifort", "fpm", "clang", "cmake",
                     "make", "gasnetrun_smp", "caf", "cafrun", "lfortran", "sed"):
            target = self.bin / name
            target.write_text(mock)
            target.chmod(0o755)

    def git_run(self, *args):
        return subprocess.check_output(
            [self.git, *args], cwd=self.directory, text=True, stderr=subprocess.STDOUT,
        ).strip()

    def commit(self, *paths):
        self.git_run("add", "--", *paths)
        self.git_run("commit", "-qm", "CI fixture")
        return self.git_run("rev-parse", "HEAD")

    def run_caffeine(self, scope="quick", base=None, **env):
        self.calls.write_text("")
        environment = dict(self.env, LFORTRAN_COARRAY_BASE=self.base if base is None else base,
                           LFORTRAN_COARRAY_HEAD=self.git_run("rev-parse", "HEAD"), **env)
        if scope is not None:
            environment["LFORTRAN_CI_SCOPE"] = scope
        result = subprocess.run(
            ["bash", str(ROOT / "ci/test_caffeine.sh"), "--verify-all-passes"],
            cwd=self.directory, env=environment, capture_output=True, text=True,
        )
        calls = [json.loads(line) for line in self.calls.read_text().splitlines()]
        return result, calls

    def assert_capability_coverage(self, calls, default_images="2"):
        unit = [call for call in calls if call["key"] == "unit"]
        self.assertEqual(len(unit), 1)
        self.assertEqual(unit[0]["args"], ["test", "--verbose"])
        self.assertEqual(unit[0]["env"],
                         dict(FC="lfortran", CC="clang", CXX="clang++", CAF_IMAGES="4"))
        install = next(call for call in calls if call["key"] == "install.sh")
        self.assertNotIn("--disable-fpm", install["args"])
        smoke = next(call for call in calls if call["command"] == "make")
        self.assertEqual(smoke["args"], ["-C", "caffeine/app", "prif"])
        self.assertLess(calls.index(unit[0]), calls.index(smoke))
        compiles = [call for call in calls if call["key"] == "lfortran"]
        self.assertEqual(len(compiles), 3)
        for call, name in zip(compiles, ("coarrays_03", "coarrays_51", "coarrays_11")):
            self.assertIn(f"integration_tests/{name}.f90", call["args"])
            self.assertIn("--verify-all-passes", call["args"])
            self.assertIn("--coarray=true", call["args"])
            self.assertNotIn("--smoke", call["args"])
        self.assertIn("integration_tests/coarrays_51_m.f90", compiles[1]["args"])
        self.assertIn("--separate-compilation", compiles[1]["args"])
        runs = [call["args"] for call in calls if call["command"] == "gasnetrun_smp"]
        self.assertEqual(runs, [
            ["-n", default_images, "./coarrays_03_lf.out"],
            ["-n", "4", "./coarrays_51_lf.out"],
            ["-n", "6", "./coarrays_11_lf.out"],
        ])

    def test_quick_skips_only_reference_work_for_unchanged_inputs(self):
        sentinel = self.directory / "OpenCoarrays"
        sentinel.mkdir()
        (sentinel / "unowned").touch()
        (self.directory / "src").mkdir()
        (self.directory / "src/compiler.cpp").write_text("// compiler-only change\n")
        self.commit("src/compiler.cpp")
        result, calls = self.run_caffeine(CAF_IMAGES="3", LFORTRAN_TEST_SUITE="smoke")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assert_capability_coverage(calls, default_images="3")
        self.assertFalse({call["command"] for call in calls} &
                         {"micromamba", "mpifort", "cmake", "caf", "cafrun"})
        self.assertTrue((sentinel / "unowned").exists())
        self.assertIn("unchanged", result.stdout + result.stderr)

    def test_full_or_changed_inputs_keep_reference_validation(self):
        source = self.directory / "integration_tests/coarrays_51_m.f90"
        source.write_text(source.read_text() + "! edited support file\n")
        self.commit("integration_tests/coarrays_51_m.f90")
        for scope, base in ((None, None), ("exhaustive", None), ("quick", None), ("quick", "")):
            with self.subTest(scope=scope, base=base):
                result, calls = self.run_caffeine(scope, base)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assert_capability_coverage(calls)
                references = [call for call in calls if call["command"] == "caf" and
                              call["args"] != ["--version"]]
                self.assertEqual(len(references), 2)
                self.assertIn("integration_tests/coarrays_51_m.f90", references[1]["args"])
                self.assertEqual([call["args"] for call in calls if call["command"] == "cafrun" and
                                  call["args"] != ["--version"]],
                                 [["-np", "2", "./coarrays_03_gf.out"],
                                  ["-np", "4", "./coarrays_51_gf.out"]])
                self.assertTrue(any(call["command"] == "cmake" for call in calls))
                self.assertFalse((self.directory / "OpenCoarrays").exists())

    def test_macos_still_runs_unit_and_capability_tests_without_opencoarrays(self):
        result, calls = self.run_caffeine("exhaustive", CI_OS="Darwin")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assert_capability_coverage(calls)
        self.assertFalse({call["command"] for call in calls} &
                         {"mpifort", "cmake", "caf", "cafrun"})

    def test_missing_fpm_installs_the_existing_pin_before_unit_tests(self):
        result, calls = self.run_caffeine(CI_FPM_PRESENT="false")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assert_capability_coverage(calls)
        installs = [call for call in calls if call["command"] == "micromamba"]
        self.assertEqual([call["args"] for call in installs],
                         [["install", "-y", "-c", "conda-forge", "fpm=0.12.0"]])
        versions = [call for call in calls if call["command"] == "fpm"]
        self.assertEqual(len(versions), 2)
        self.assertLess(calls.index(versions[0]), calls.index(installs[0]))

    def test_reference_probe_does_not_require_mpi_build_time_compiler(self):
        for scope in ("quick", "exhaustive"):
            for present in ("true", "false"):
                with self.subTest(scope=scope, mpi_present=present):
                    (self.directory / "mpifort-installed").unlink(missing_ok=True)
                    result, calls = self.run_caffeine(
                        scope, base="", CI_MPI_PRESENT=present,
                        CI_MPI_BUILD_COMPILER_MISSING="true",
                    )
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    self.assert_capability_coverage(calls)
                    probes = [call["args"] for call in calls if call["command"] == "mpifort"]
                    self.assertEqual(probes, [["--showme:version"]] * (1 if present == "true" else 2))
                    installs = [call["args"] for call in calls if call["command"] == "micromamba"]
                    self.assertEqual(installs, [] if present == "true" else [
                        ["install", "-y", "-c", "conda-forge", "openmpi=5.0.6=hb85ec53_102"],
                    ])
                    references = [call for call in calls if call["command"] == "caf" and
                                  call["args"] != ["--version"]]
                    self.assertEqual(len(references), 2)

    def test_unit_failure_stops_before_smoke_and_integration(self):
        result, calls = self.run_caffeine(CI_FAIL="unit")
        self.assertEqual(result.returncode, 42, result.stdout + result.stderr)
        self.assertFalse({call["key"] for call in calls} & {"make", "lfortran", "gasnetrun_smp"})

    def test_dependency_and_capability_failures_are_not_successes(self):
        cases = (
            ("fpm", "quick", "true"), ("micromamba", "quick", "false"),
            ("install.sh", "quick", "true"), ("info", "quick", "true"),
            ("make", "quick", "true"), ("lfortran", "quick", "true"),
            ("gasnetrun_smp", "quick", "true"), ("mpifort", "exhaustive", "true"),
            ("cmake", "exhaustive", "true"), ("caf", "exhaustive", "true"),
            ("cafrun", "exhaustive", "true"), ("sed", "exhaustive", "true"),
        )
        for failure, scope, present in cases:
            with self.subTest(failure=failure):
                for path in ("caffeine", "OpenCoarrays"):
                    shutil.rmtree(self.directory / path, ignore_errors=True)
                result, calls = self.run_caffeine(scope, CI_FAIL=failure, CI_FPM_PRESENT=present)
                self.assertEqual(result.returncode, 42, result.stdout + result.stderr)
                self.assertNotIn("All coarray runtime tests passed", result.stdout)
                if failure == "fpm":
                    self.assertFalse(any(call["command"] == "micromamba" for call in calls))

    def select_reference(self, base=None, head=None, **env):
        return subprocess.run(
            [sys.executable, str(ROOT / "ci/coarray_tests.py"), "reference",
             "--base", self.base if base is None else base,
             "--head", self.git_run("rev-parse", "HEAD") if head is None else head],
            cwd=self.directory, env=dict(self.env, **env), capture_output=True, text=True,
        )

    def test_reference_selection_tracks_edits_additions_renames_and_deletions(self):
        changes = (
            ("integration_tests/coarrays_03.f90", "! primary edit\n"),
            ("integration_tests/coarrays_51_m.f90", "! support edit\n"),
            ("ci/test_caffeine.sh", "# harness edit\n"),
            ("ci/coarray_tests.py", "# selector edit\n"),
            ("ci/environment_linux_llvm.yml", "# dependency edit\n"),
            ("ci/environment.yml", "dependencies: []\n"),
            (".github/workflows/Compiler-Compatibility-CI.yml", "# workflow edit\n"),
            (".github/workflows/Quick-Checks-CI.yml", "# caller edit\n"),
            ("integration_tests/unrelated.f90", "! unrelated integration edit\n"),
            ("src/compiler.cpp", "// compiler edit\n"),
        )
        for path, addition in changes:
            with self.subTest(path=path):
                target = self.directory / path
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text((target.read_text() if target.exists() else "") + addition)
                head = self.commit(path)
                result = self.select_reference()
                self.assertEqual(result.returncode, 0, result.stderr)
                expected = path not in ("integration_tests/unrelated.f90", "src/compiler.cpp")
                self.assertEqual(result.stdout, f"{str(expected).lower()}\n", result.stderr)
                self.base = head
        for action in ("rename", "delete"):
            with self.subTest(action=action):
                path = "integration_tests/coarrays_03.f90"
                if action == "rename":
                    self.git_run("mv", path, "integration_tests/renamed.f90")
                else:
                    self.git_run("rm", "integration_tests/coarrays_51_m.f90")
                self.git_run("commit", "-qm", "Change coarray input")
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.base = self.git_run("rev-parse", "HEAD")

    def test_only_relevant_manifest_changes_request_reference_validation(self):
        original = self.manifest.read_text()
        changes = (
            (original + "RUN(NAME another LABELS llvm)\n", False),
            (original.replace("NUM_IMAGES=4", "NUM_IMAGES=8"), True),
            (original.replace("--separate-compilation", "--separate-compilation --fast"), True),
            (original.replace("coarrays_51_m.f90", "different_support.f90"), True),
            (original + "RUN(NAME new_coarray EXTRA_ARGS --coarray=true)\n", True),
            (original.replace("RUN(NAME coarrays_03 EXTRA_ARGS --coarray=true)\n", ""), True),
            (original.replace("coarrays_03", "renamed"), True),
            (original + "include(coarray_inputs.cmake)\n", True),
            (original + "macro(RUN)\nmessage(STATUS changed)\nendmacro(RUN)\n", True),
        )
        for manifest, expected in changes:
            with self.subTest(manifest=manifest):
                self.manifest.write_text(manifest)
                self.commit("integration_tests/CMakeLists.txt")
                result = self.select_reference()
                self.assertEqual(result.stdout, f"{str(expected).lower()}\n", result.stderr)
                self.manifest.write_text(original)
                self.base = self.commit("integration_tests/CMakeLists.txt")

    def test_missing_history_dirty_checkout_and_git_failures_are_conservative(self):
        for base, head in (("", None), ("0" * 40, None), ("f" * 40, None),
                           (None, ""), (None, "0" * 40), (None, "f" * 40)):
            with self.subTest(base=base, head=head):
                result = self.select_reference(base, head)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "true\n")
                self.assertIn("conservative", result.stderr)
        result = self.select_reference(CI_FAIL="git")
        self.assertEqual(result.stdout, "true\n")
        self.assertIn("conservative", result.stderr)
        self.manifest.write_text(self.manifest.read_text() + "# uncommitted edit\n")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n")
        self.assertIn("conservative", result.stderr)
        self.commit("integration_tests/CMakeLists.txt")
        result = self.select_reference(head=self.base)
        self.assertEqual(result.stdout, "true\n")
        self.assertIn("checkout does not match", result.stderr)

    def test_quick_reference_selection_is_event_independent(self):
        for event in ("pull_request", "push", "workflow_dispatch"):
            with self.subTest(event=event):
                result = self.select_reference(GITHUB_EVENT_NAME=event)
                self.assertEqual(result.stdout, "false\n", result.stderr)
                self.assertIn("unchanged", result.stderr)
                result = self.select_reference(base="", GITHUB_EVENT_NAME=event)
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)

    def test_untracked_dependencies_never_silently_disable_reference_validation(self):
        source = self.directory / "integration_tests/coarrays_03.f90"
        for dependency in ("include 'shared.inc'", '#include "shared.h"',
                           "open(10, file='input.dat')", "use other_module",
                           "#define IMPORT use", "open &\n(10, file='input.dat')",
                           "inquire(file='input.dat', exist=exists)",
                           "write(10, *) value", "close(10, status='delete')",
                           "flush(10)", "rewind(10)", "backspace(10)",
                           "endfile(10)", "wait(10)",
                           "subroutine input() bind(c)",
                           "call get_environment_variable('DATA', data)"):
            with self.subTest(dependency=dependency):
                source.write_text("program coarrays_03\n" + dependency + "\nend program\n")
                self.base = self.commit("integration_tests/coarrays_03.f90")
                (self.directory / "input.dat").write_text("changed external input\n")
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("dependencies", result.stderr)

    def test_changed_data_with_quoted_comments_or_continued_keywords_runs_reference(self):
        source = self.directory / "integration_tests/coarrays_03.f90"
        data = self.directory / "input.dat"
        cases = (
            ("ordinary", "open(unit=10, file='input.dat')\nread(10, *) value"),
            ("quoted-exclamation",
             "print *, '!'; open(unit=10, file='input.dat')\n"
             "print *, '!'; read(10, *) value"),
            ("continued-keywords", "op&\n&en(unit=10, file='input.dat')\nre&\n&ad(10, *) value"),
            ("continued-with-comments",
             "op& ! continued token\n! comment between lines\n"
             "&en(unit=10, file='input.dat')\nre&\n&ad(10, *) value"),
        )
        for name, statements in cases:
            with self.subTest(case=name):
                source.write_text(
                    "program coarrays_03\ninteger :: value[*]\n" + statements +
                    "\nclose(10)\nsync all\nif (value /= 42) error stop\nend program\n"
                )
                data.write_text("41\n")
                head = self.commit("integration_tests/coarrays_03.f90", "input.dat")
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("changed coarray sources", result.stderr)
                self.base = head
                data.write_text("42\n")
                self.commit("input.dat")
                self.assertEqual(self.git_run("diff", "--name-only", self.base, "HEAD"), "input.dat")
                result = self.select_reference()
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)
                result, calls = self.run_caffeine()
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assert_capability_coverage(calls)
                self.assertIn("Run GFortran/OpenCoarrays reference validation: true", result.stdout)
                for command in ("mpifort", "caf", "cafrun"):
                    self.assertTrue(any(call["command"] == command for call in calls))
                self.base = self.git_run("rev-parse", "HEAD")

    def test_inquire_and_fixed_form_data_changes_run_reference(self):
        source = self.directory / "integration_tests/coarrays_03.f90"
        data = self.directory / "input.dat"
        original = self.manifest.read_text()
        support = self.directory / "integration_tests/input_support.f"
        support.write_text(
            "      subroutine load_value(value)\n"
            "      implicit none\n"
            "      integer :: value\n"
            "      o p e n(unit=10, file='input.dat')\n"
            "      r e a d(10, *) value\n"
            "      close(10)\n"
            "      end\n"
        )
        for name, statements, before, after in (
            ("inquire", "inquire(file='input.dat', size=value)", "42\n", "420\n"),
            ("computed-inquire",
             "character(9) :: path\npath = 'input' // '.dat'\n"
             "inquire(file=path, size=value)", "42\n", "420\n"),
            ("fixed-form-support", "call load_value(value)", "3\n", "4\n"),
        ):
            with self.subTest(case=name):
                self.manifest.write_text(original.replace(
                    "RUN(NAME coarrays_03 EXTRA_ARGS --coarray=true)",
                    "RUN(NAME coarrays_03 EXTRA_ARGS --coarray=true EXTRAFILES input_support.f)"
                    if name == "fixed-form-support" else
                    "RUN(NAME coarrays_03 EXTRA_ARGS --coarray=true)",
                ))
                source.write_text(
                    "program coarrays_03\nimplicit none\ninteger :: value[*]\n" + statements +
                    "\nprint *, value\nsync all\nif (value /= 3) error stop 1\nend program\n"
                )
                data.write_text(before)
                self.base = self.commit("integration_tests", "input.dat")
                data.write_text(after)
                self.commit("input.dat")
                self.assertEqual(self.git_run("diff", "--name-only", self.base, "HEAD"), "input.dat")
                result = self.select_reference()
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)
                result, calls = self.run_caffeine()
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assert_capability_coverage(calls)
                for command in ("mpifort", "caf", "cafrun"):
                    self.assertTrue(any(call["command"] == command for call in calls))

    def test_file_queries_do_not_make_other_sources_unrelated(self):
        source = self.directory / "integration_tests/coarrays_03.f90"
        for path in ("src/compiler.cpp", "integration_tests/unrelated.f90"):
            with self.subTest(path=path):
                data = self.directory / path
                data.parent.mkdir(parents=True, exist_ok=True)
                if not data.exists():
                    data.write_text("// compiler source\n")
                source.write_text(
                    "program coarrays_03\ninteger :: value[*]\n"
                    f"inquire(file='{path}', size=value)\n"
                    "sync all\nend program\n"
                )
                self.base = self.commit("integration_tests/coarrays_03.f90", path)
                data.write_text(data.read_text() + "\n")
                self.commit(path)
                self.assertEqual(self.git_run("diff", "--name-only", self.base, "HEAD"), path)
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)

    def test_data_support_and_unknown_paths_are_conservative_without_source_hints(self):
        paths = (
            "input.dat", "input", "input with spaces.bin", "input.f90", "compiler.cpp",
            "integration_tests/input.dat", "integration_tests/data/input.csv",
            "integration_tests/shared.inc", "integration_tests/input_support.f",
            "integration_tests/preprocessed_support.F90", "integration_tests/input_support.c",
            "integration_tests/input_support.f90", "tests/input.txt", "src/input.dat",
            "settings.json",
        )
        for path in paths:
            with self.subTest(path=path):
                target = self.directory / path
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text("unidentified input\n")
                head = self.commit(path)
                result = self.select_reference()
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)
                self.base = head
        data = self.directory / "input.dat"
        for action in ("edit", "rename", "delete"):
            with self.subTest(action=action):
                if action == "edit":
                    data.write_text("changed input\n")
                    self.commit("input.dat")
                elif action == "rename":
                    self.git_run("mv", "input.dat", "renamed.dat")
                    self.git_run("commit", "-qm", "Rename data")
                else:
                    self.git_run("rm", "renamed.dat")
                    self.git_run("commit", "-qm", "Delete data")
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)
                self.base = self.git_run("rev-parse", "HEAD")

    def test_unknown_coarray_source_forms_and_options_are_conservative(self):
        original = self.manifest.read_text()
        for index, (suffix, option) in enumerate((
            (".f", ""), (".F", ""), (".for", ""), (".F90", ""), (".c", ""), (".inc", ""),
            (".f90", "--fixed-form"), (".f90", "--cpp"), (".f90", "-Iincludes"),
        )):
            with self.subTest(suffix=suffix, option=option):
                support = f"input_support_{index}{suffix}"
                (self.directory / "integration_tests" / support).write_text("! support source\n")
                self.manifest.write_text(original.replace(
                    "--coarray=true)", f"--coarray=true {option} EXTRAFILES {support})",
                ))
                self.base = self.commit("integration_tests")
                result = self.select_reference()
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)

    def test_only_simple_standalone_noncoarray_source_changes_can_skip_reference(self):
        source = self.directory / "integration_tests/unrelated.f90"
        original = source.read_text()
        for body, expected in (
            ("program unrelated\ninteger :: x\nx = 1\nend program\n", False),
            ("module support\nend module\n", True),
            ("subroutine support\nend subroutine\n", True),
            ("function support()\nend function\n", True),
            ("program unrelated\nend program\nmodule support\nend module\n", True),
            ("program unrelated\ninclude 'shared.inc'\nend program\n", True),
            ("#include \"shared.inc\"\nprogram unrelated\nend program\n", True),
            ("program unrelated\ncontains\nsubroutine sub()\nend subroutine\nend program\n", True),
            ("program unre&\n&lated\nend program\n", True),
        ):
            with self.subTest(body=body):
                source.write_text(body)
                self.commit("integration_tests/unrelated.f90")
                result = self.select_reference()
                self.assertEqual(result.stdout, f"{str(expected).lower()}\n", result.stderr)
                source.write_text(original)
                self.base = self.commit("integration_tests/unrelated.f90")
        source.write_text("module support\nend module\n")
        self.base = self.commit("integration_tests/unrelated.f90")
        source.write_text(original)
        self.commit("integration_tests/unrelated.f90")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n", result.stderr)

    def test_ambiguous_unrelated_source_files_and_read_failures_are_conservative(self):
        source = self.directory / "integration_tests/unrelated.f90"
        source.write_bytes(b"\xff\n")
        self.commit("integration_tests/unrelated.f90")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n", result.stderr)
        self.assertIn("decode", result.stderr)
        source.unlink()
        source.symlink_to("coarrays_03.f90")
        self.commit("integration_tests/unrelated.f90")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n", result.stderr)
        self.assertIn("non-regular", result.stderr)

    def test_simple_noncoarray_additions_renames_and_deletions_keep_the_fast_path(self):
        source = self.directory / "integration_tests/another.f90"
        registration = "RUN(NAME another LABELS gfortran llvm)\n"
        source.write_text("program another\nend program another\n")
        self.manifest.write_text(self.manifest.read_text() + registration)
        self.commit("integration_tests")
        result = self.select_reference()
        self.assertEqual(result.stdout, "false\n", result.stderr)
        self.base = self.git_run("rev-parse", "HEAD")
        self.git_run("mv", "integration_tests/another.f90", "integration_tests/renamed.f90")
        self.manifest.write_text(self.manifest.read_text().replace(registration, registration.replace(
            "another", "renamed",
        )))
        self.commit("integration_tests")
        result = self.select_reference()
        self.assertEqual(result.stdout, "false\n", result.stderr)
        self.base = self.git_run("rev-parse", "HEAD")
        self.git_run("rm", "integration_tests/renamed.f90")
        self.manifest.write_text(self.manifest.read_text().replace(
            registration.replace("another", "renamed"), "",
        ))
        self.commit("integration_tests")
        result = self.select_reference()
        self.assertEqual(result.stdout, "false\n", result.stderr)

    def test_ambiguous_module_dependencies_are_conservative(self):
        source = self.directory / "integration_tests/coarrays_03.f90"
        cases = (
            "print *, '!'; use other_module",
            "us&\n&e other_module",
            "use &\nother_module",
            "use other_module ! ; module other_module",
            "use other_module\nprint *, '; module other_module'",
            "100 use other_module",
            "submodule(other_module) child",
        )
        for statements in cases:
            with self.subTest(statements=statements):
                source.write_text(statements + "\n")
                self.base = self.commit("integration_tests/coarrays_03.f90")
                (self.directory / "other_module.f90").write_text(statements + "\n")
                self.commit("other_module.f90")
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n", result.stderr)
                self.assertIn("conservative", result.stderr)

    def test_actual_coarray_sources_keep_the_compiler_only_fast_path(self):
        spec = importlib.util.spec_from_file_location("coarray_tests", ROOT / "ci/coarray_tests.py")
        helper = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(helper)
        manifest = (INTEGRATION / "CMakeLists.txt").read_text()
        self.manifest.write_text(manifest)
        for test in helper.parse_tests(manifest):
            for source in (test[0], *test[3].split()):
                shutil.copyfile(ROOT / source, self.directory / source)
        unrelated = self.directory / "integration_tests/expr_02.f90"
        shutil.copyfile(INTEGRATION / "expr_02.f90", unrelated)
        self.base = self.commit("integration_tests")
        (self.directory / "src").mkdir()
        (self.directory / "src/compiler.cpp").write_text("// compiler-only change\n")
        self.commit("src/compiler.cpp")
        result = self.select_reference()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "false\n", result.stderr)
        self.assertIn("unchanged", result.stderr)
        self.base = self.git_run("rev-parse", "HEAD")
        unrelated.write_text(unrelated.read_text() + "! unrelated regression edit\n")
        self.commit("integration_tests/expr_02.f90")
        result = self.select_reference()
        self.assertEqual(result.stdout, "false\n", result.stderr)
        self.assertIn("unchanged", result.stderr)

    def test_diff_and_source_read_errors_never_report_unchanged(self):
        for command in ("diff", "show", "ls-tree"):
            with self.subTest(command=command):
                result = self.select_reference(CI_GIT_FAIL=command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "true\n")
                self.assertIn("conservative", result.stderr)
                self.assertIn("42", result.stderr)
        result, calls = self.run_caffeine(CI_GIT_FAIL="diff")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("conservative", result.stderr)
        self.assertTrue(any(call["command"] == "cafrun" for call in calls))
        self.assert_capability_coverage(calls)
        source = self.directory / "integration_tests/coarrays_03.f90"
        source.write_bytes(b"\xff\n")
        self.base = self.commit("integration_tests/coarrays_03.f90")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n", result.stderr)
        self.assertIn("decode", result.stderr)

    def test_invalid_registrations_surface_errors_before_setup(self):
        for manifest in ("RUN(NAME ${test} EXTRA_ARGS --coarray=true)\n",
                         'RUN(NAME coarrays_03 EXTRA_ARGS --coarray=true ")\n'):
            with self.subTest(manifest=manifest):
                self.manifest.write_text(manifest)
                self.base = self.commit("integration_tests/CMakeLists.txt")
                result = self.select_reference()
                self.assertEqual(result.stdout, "true\n")
                self.assertIn("conservative", result.stderr)
                result, calls = self.run_caffeine()
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("ValueError", result.stderr)
                self.assertFalse({call["key"] for call in calls} &
                                 {"micromamba", "install.sh", "unit", "lfortran"})

    def test_unknown_cmake_includes_remain_conservative_when_the_manifest_is_unchanged(self):
        self.manifest.write_text(self.manifest.read_text() + "include(test_inputs.cmake)\n")
        include = self.directory / "integration_tests/test_inputs.cmake"
        include.write_text("# input dependency\n")
        self.base = self.commit("integration_tests")
        include.write_text("# changed input dependency\n")
        self.commit("integration_tests/test_inputs.cmake")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n")
        self.assertIn("CMake dependencies", result.stderr)

    def test_all_registered_sources_match_the_existing_manifest_selection(self):
        spec = importlib.util.spec_from_file_location("coarray_tests", ROOT / "ci/coarray_tests.py")
        helper = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(helper)
        source = (INTEGRATION / "CMakeLists.txt").read_text()
        expected = []
        for line in source.splitlines():
            if line.strip().startswith("RUN(") and "coarray=true" in line:
                fields = "NAME|NUM_IMAGES|LABELS|EXTRAFILES|EXTRA_ARGS"
                data = dict(re.findall(rf"({fields})[ =]\s*(.*?)(?=\s+(?:{fields})[ =]|\))", line))
                expected.append((
                    f"integration_tests/{data['NAME']}.f90", data.get("NUM_IMAGES", ""),
                    data.get("EXTRA_ARGS", ""),
                    " ".join(f"integration_tests/{path}" for path in data.get("EXTRAFILES", "").split()),
                ))
        self.assertTrue(expected)
        self.assertEqual([test[:4] for test in helper.parse_tests(source)], expected)
        parsed = helper.parse_tests(
            "# RUN(NAME ignored EXTRA_ARGS --coarray=true)\n"
            "RUN(\n NAME multi\n EXTRA_ARGS --coarray=true\n EXTRAFILES helper.f90\n NUM_IMAGES 4)\n"
        )
        self.assertEqual(parsed, [
            ("integration_tests/multi.f90", "4", "--coarray=true", "integration_tests/helper.f90", ())
        ])
        for invalid in ("RUN(NAME ${name} EXTRA_ARGS --coarray=true)",
                        "RUN(NAME a COPY_TO_BIN data EXTRA_ARGS --coarray=true)",
                        "RUN(NAME a EXTRA_ARGS --coarray=true NUM_IMAGES=0)",
                        "RUN(NAME a EXTRA_ARGS --coarray=true EXTRAFILES ../outside.f90)",
                        "RUN(NAME a EXTRA_ARGS --coarray=true)\nRUN(NAME a EXTRA_ARGS --coarray=true)"):
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                helper.parse_tests(invalid)

    def test_bad_scope_or_manifest_fails_before_dependency_setup(self):
        result, calls = self.run_caffeine("unknown")
        self.assertEqual(result.returncode, 2)
        self.assertFalse(any(call["command"] == "micromamba" for call in calls))
        self.manifest.write_text("RUN(NAME unrelated LABELS llvm)\n")
        result, calls = self.run_caffeine("exhaustive")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no coarray tests found", result.stderr)
        self.assertFalse(any(call["command"] == "micromamba" for call in calls))

    def test_failed_selector_does_not_silently_skip_reference_work(self):
        helper = self.directory / "ci/coarray_tests.py"
        for script, expected in (("import sys; sys.exit(42)\n", 42),
                                 ("print('unexpected')\n", 2)):
            with self.subTest(script=script):
                helper.write_text(script)
                result, calls = self.run_caffeine()
                self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
                self.assertFalse({call["key"] for call in calls} &
                                 {"micromamba", "install.sh", "unit", "lfortran"})

    def test_nonancestor_history_is_conservative(self):
        self.git_run("checkout", "-q", "--orphan", "new-history")
        self.git_run("commit", "-qm", "New root")
        result = self.select_reference()
        self.assertEqual(result.stdout, "true\n")
        self.assertIn("conservative", result.stderr)


if __name__ == "__main__":
    unittest.main()
