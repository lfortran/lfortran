#!/usr/bin/env bash
# Temporary first-fault diagnostics for PR #16329; retain the original test status.
set -euo pipefail

exe=$(realpath "$1")
test_cwd=$(realpath "$2")
shift 2
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
out=${LFORTRAN_DIAGNOSTIC_DIR:-"$root/.fix-issue/pr-16329/ci-llvm11-unit"}
mkdir -p "$out"
out=$(realpath "$out")
printf 'Original command:' > "$out/command.log"
printf ' %q' "$@" >> "$out/command.log"
printf '\nOriginal cwd: %s\nDebugger cwd: %s\nExecutable: %s\n' \
    "$PWD" "$test_cwd" "$exe" >> "$out/command.log"
cat "$out/command.log"

normal_status=0
"$@" > "$out/normal.log" 2>&1 || normal_status=$?
cat "$out/normal.log"
printf 'NORMAL_EXIT=%s\n' "$normal_status" > "$out/status.log"

identity_status=0
(
    printf 'GITHUB_SHA=%s\n' "${GITHUB_SHA:-local}"
    git -C "$root" show -s --format='CHECKOUT=%H%nPARENTS=%P%nSUBJECT=%s'
    uname -a
    cat /etc/os-release
    getconf GNU_LIBC_VERSION
    gcc --version
    c++ --version
    ld --version
    llvm-config --version
    command -v gdb
    gdb --version
    printf 'LD_PRELOAD=%s\nLD_LIBRARY_PATH=%s\n' \
        "${LD_PRELOAD-}" "${LD_LIBRARY_PATH-}"
    printf 'CORE_PATTERN='
    cat /proc/sys/kernel/core_pattern
    if [[ -f CMakeCache.txt ]]; then
        grep -E '^(CMAKE_(BUILD_TYPE|C_COMPILER|CXX_COMPILER|C_FLAGS|CXX_FLAGS)|WITH_(LLVM|XEUS|LSP|STACKTRACE|RUNTIME_STACKTRACE|INTERNAL_ALLOC_CHECK)|LLVM_DIR)[:_]' CMakeCache.txt
    fi
    sha256sum "$exe"
    readelf -W -d -n "$exe"
    ldd "$exe" > "$out/ldd.log"
    cat "$out/ldd.log"
    while read -r library; do
        readlink -f "$library"
        sha256sum "$library"
        readelf -W -d -n "$library"
    done < <(awk '/=> \// {print $3} /^[[:space:]]*\/.*\(/ {print $1}' \
        "$out/ldd.log" | sort -u)
) > "$out/identity.log" 2>&1 || identity_status=$?
cat "$out/identity.log"

gdb_status=0
(
    cd "$test_cwd"
    LFORTRAN_UNIT_DIAGNOSTICS="$out" timeout --kill-after=15s 300s \
        gdb -nx -q -batch -x "$root/ci/diagnose_llvm11_unit.gdb" --args "$exe"
) > "$out/gdb.log" 2>&1 || gdb_status=$?
cat "$out/gdb.log"

# Keep loader tracing out of the first-fault debugger run: it changes timing.
loader_status=0
(
    cd "$test_cwd"
    timeout --kill-after=15s 300s env \
        LD_DEBUG=libs,bindings,versions LD_DEBUG_OUTPUT="$out/loader" "$exe"
) > "$out/loader-test.log" 2>&1 || loader_status=$?
cat "$out/loader-test.log"
grep -hE 'binding file .*(_Unwind|__cxa_throw|__cxa_rethrow|__gxx_personality_v0|__register_frame|__deregister_frame|_dl_find_object)' \
    "$out"/loader.* > "$out/bindings.log" || true
head -120 "$out/bindings.log"
printf 'IDENTITY_EXIT=%s\nGDB_EXIT=%s\nLOADER_EXIT=%s\n' \
    "$identity_status" "$gdb_status" "$loader_status" >> "$out/status.log"
cat "$out/status.log"

if (( normal_status != 0 )); then
    exit "$normal_status"
fi
if (( identity_status != 0 )); then
    exit "$identity_status"
fi
if (( gdb_status != 0 )); then
    exit "$gdb_status"
fi
exit "$loader_status"
