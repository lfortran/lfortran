#!/usr/bin/env python3
"""Share Caffeine's registered test inputs with Quick's reference selection."""

import argparse
from pathlib import Path, PurePosixPath
import re
import shlex
import subprocess
import sys


MANIFEST = "integration_tests/CMakeLists.txt"
HARNESS_INPUTS = {
    "ci/coarray_tests.py",
    "ci/test_caffeine.sh",
    "ci/test_third_party_codes.sh",
    "integration_tests/run_tests.py",
    ".github/workflows/Compiler-Compatibility-CI.yml",
    ".github/workflows/Quick-Checks-CI.yml",
}
COMPILER_SUFFIXES = {".asdl", ".c", ".cpp", ".h", ".hpp", ".py", ".re", ".yy"}


def parse_tests(source):
    # These are literal RUN registrations, not a general CMake interpreter.
    source = re.sub(r"#[^\n]*", "", source)
    bodies = re.findall(r"\bRUN\s*\(([^()]*)\)", source)
    if sum(body.count("coarray=true") for body in bodies) != source.count("coarray=true"):
        raise ValueError("coarray tests must use literal RUN(...) registrations")
    tests = []
    names = set()
    for body in bodies:
        if "coarray=true" not in body:
            continue
        fields = {key: [] for key in ("NAME", "NUM_IMAGES", "LABELS", "EXTRAFILES", "EXTRA_ARGS")}
        key = None
        for token in shlex.split(body):
            field, separator, value = token.partition("=")
            if field in fields:
                key = field
                if separator:
                    fields[key].append(value)
            elif key is None or re.fullmatch(r"[A-Z][A-Z_]+", field):
                raise ValueError(f"unsupported coarray registration field: {token}")
            else:
                fields[key].append(token)
        if len(fields["NAME"]) != 1 or "--coarray=true" not in fields["EXTRA_ARGS"]:
            raise ValueError(f"invalid coarray registration: {body}")
        name = fields["NAME"][0]
        if name in names:
            raise ValueError(f"duplicate coarray test: {name}")
        names.add(name)
        images = fields["NUM_IMAGES"]
        if images and (len(images) != 1 or not re.fullmatch(r"[1-9][0-9]*", images[0])):
            raise ValueError(f"invalid image count for {name}")
        for token in (item for values in fields.values() for item in values):
            if not re.fullmatch(r"[\w./=:+,\-]+", token):
                raise ValueError(f"unsupported coarray test argument: {token}")
        sources = [name + ".f90", *fields["EXTRAFILES"]]
        if any(PurePosixPath(path).is_absolute() or ".." in PurePosixPath(path).parts
               for path in sources):
            raise ValueError(f"coarray sources must be inside integration_tests: {name}")
        tests.append((
            f"integration_tests/{name}.f90", "".join(images),
            " ".join(fields["EXTRA_ARGS"]),
            " ".join(f"integration_tests/{path}" for path in fields["EXTRAFILES"]),
            tuple(fields["LABELS"]),
        ))
    return tests


def git(*arguments):
    return subprocess.check_output(["git", *arguments], stderr=subprocess.PIPE).decode()


def manifest_dependencies(source):
    source = re.sub(r"#[^\n]*", "", source)
    return re.findall(
        r"(?ims)^\s*macro\s*\(RUN(?:_UTIL)?(?:\s[^)]*)?\).*?^\s*endmacro\s*\([^)]*\)"
        r"|^\s*include\s*\([^)]*\)", source,
    )


def source_text(head, sources):
    tree = git("ls-tree", head, "--", *sources).splitlines()
    if len(tree) != len(sources) or any(not line.startswith(("100644 ", "100755 ")) for line in tree):
        raise ValueError("missing or non-regular reference source")
    code = "\n".join(git("show", f"{head}:{path}") for path in sources)
    # This is a conservative guard, not a Fortran lexer. Only whole comment
    # lines are safe to discard: a quoted '!' can precede executable statements.
    # Keep strings and trailing comments, accepting false positives.
    return re.sub(r"(?m)^[ \t]*![^\n]*", "", code)


def check_source_dependencies(head, test):
    sources = [test[0], *test[3].split()]
    if any(PurePosixPath(path).suffix != ".f90" for path in sources):
        raise ValueError("unknown coarray source forms/languages need reference validation")
    if set(test[2].split()) - {"--coarray=true", "--separate-compilation"}:
        raise ValueError("unknown coarray source options need reference validation")
    code = source_text(head, sources)
    # Do not try to reconstruct split tokens or continued character literals.
    # Ordinary continuation between tokens (e.g. after a comma) remains safe.
    if re.search(r"(?m)\w&|^[ \t]*&", code):
        raise ValueError("continued coarray tokens may hide dependencies")
    if re.search(r"(?im)^\s*#|\b(?:include|open|read|inquire|close|backspace|endfile|"
                 r"rewind|flush|wait|bind|submodule|get_command_argument|"
                 r"get_environment_variable|execute_command_line)\b"
                 r"|\bwrite\b(?!\s*\(\s*\*\s*,)", code):
        raise ValueError("coarray file dependencies need conservative reference validation")
    # Only trust a literal module declaration at the start of a line, not text
    # after a semicolon inside a string or trailing comment.
    modules = set(re.findall(r"(?im)^[ \t]*module[ \t]+(\w+)", code.lower()))
    uses = set(re.findall(r"(?i)\buse\s*(?:,\s*(?:non_)?intrinsic\s*::|::)?\s*(\w+)",
                          code.lower()))
    intrinsic = {"iso_fortran_env", "iso_c_binding", "ieee_arithmetic",
                 "ieee_exceptions", "ieee_features"}
    if (uses - modules - intrinsic or
            re.search(r"(?im)\buse\b[^\n]*(?:&|non_intrinsic)", code)):
        raise ValueError("unresolved coarray module dependencies need reference validation")


def unrelated_source_change(base, head, path, manifests):
    compiler = path.startswith("src/") and PurePosixPath(path).suffix in COMPILER_SUFFIXES
    standalone = (PurePosixPath(path).parent == PurePosixPath("integration_tests") and
                  PurePosixPath(path).suffix == ".f90")
    if not compiler and not standalone:
        return False
    for revision, manifest in zip((base, head), manifests):
        if not git("ls-tree", revision, "--", path).strip():
            continue  # An addition/deletion must qualify on its existing side.
        code = source_text(revision, [path])
        if compiler:
            continue
        name = re.escape(PurePosixPath(path).stem)
        registrations = re.findall(r"\bRUN\s*\(([^()]*)\)", re.sub(r"#[^\n]*", "", manifest))
        if not any(re.fullmatch(rf"\s*NAME\s+{name}\s+LABELS(?:\s+\w+)+\s*", body)
                   for body in registrations):
            return False
        # Only recognize simple main programs, never infer a support-file graph.
        # Other source layouts, preprocessing and continuations stay conservative.
        if (not re.fullmatch(r"\s*program[ \t]+(\w+)[ \t]*\n.*"
                             r"^[ \t]*end[ \t]+program(?:[ \t]+\1)?\s*",
                             code, re.IGNORECASE | re.MULTILINE | re.DOTALL) or
                re.search(r"[#&;]|\b(?:module|submodule|subroutine|function|entry|"
                          r"include|contains|interface)\b", code, re.IGNORECASE)):
            return False
    return True


def reference_required(base, head):
    try:
        if not all(re.fullmatch(r"[0-9a-fA-F]{40}", sha) and set(sha) != {"0"}
                   for sha in (base, head)):
            raise ValueError("no trustworthy base/head comparison was supplied")
        git("rev-parse", "--verify", f"{base}^{{commit}}")
        if git("rev-parse", "HEAD").strip() != head:
            raise ValueError("checkout does not match the comparison head")
        git("merge-base", "--is-ancestor", base, head)
        git("diff", "--quiet", head, "--")
        changed = set(git("diff", "--name-only", "--no-renames", "-z", base, head, "--").split("\0")) - {""}
        harness = changed & HARNESS_INPUTS
        harness.update(path for path in changed if path.startswith("ci/environment") and
                       path.endswith((".yml", ".yaml")))
        if harness:
            return True, "changed coarray harness/dependencies: " + ", ".join(sorted(harness))
        before_source = git("show", f"{base}:{MANIFEST}")
        after_source = git("show", f"{head}:{MANIFEST}")
        before, after = parse_tests(before_source), parse_tests(after_source)
        dependencies = manifest_dependencies(after_source)
        if before != after or manifest_dependencies(before_source) != dependencies:
            return True, "changed coarray registrations or CMake dependencies"
        if not after:
            raise ValueError("no coarray tests found")
        inputs = {path for test in before + after for path in (test[0], *test[3].split())}
        if changed & inputs:
            return True, "changed coarray sources: " + ", ".join(sorted(changed & inputs))
        for dependency in dependencies:
            if re.match(r"\s*include\b", dependency, re.IGNORECASE) and not re.fullmatch(
                    r'\s*include\s*\(\s*"?\$\{CMAKE_CURRENT_SOURCE_DIR\}/smoke_tests\.cmake"?\s*\)\s*',
                    dependency, re.IGNORECASE):
                raise ValueError("unresolved CMake dependencies need reference validation")
        for test in after:
            check_source_dependencies(head, test)
        # Runtime file names need not be literals, or even come from Fortran.
        # Default data, support and unknown paths to reference validation instead
        # of relying on the source guard to discover every possible dependency.
        for path in sorted(changed - {MANIFEST}):
            if not unrelated_source_change(base, head, path, (before_source, after_source)):
                raise ValueError(f"changed data, support or unknown reference inputs: {path}")
        return False, "registered coarray sources and reference inputs are unchanged"
    except (OSError, subprocess.CalledProcessError, ValueError) as error:
        return True, f"conservative reference validation: {error}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("list", help="emit the complete Caffeine integration selection")
    reference = commands.add_parser("reference", help="select Quick's GFortran/OpenCoarrays validation")
    reference.add_argument("--base", required=True)
    reference.add_argument("--head", required=True)
    args = parser.parse_args()
    if args.command == "reference":
        required, reason = reference_required(args.base, args.head)
        print(f"Coarray reference check: {reason}", file=sys.stderr)
        print("true" if required else "false")
    else:
        tests = parse_tests(Path(MANIFEST).read_text())
        if not tests:
            raise ValueError("no coarray tests found")
        for test in tests:
            print(";".join(test[:4]))


if __name__ == "__main__":
    main()
