#!/usr/bin/env python3
"""Check build/install runtime headers before and after relocation."""
import argparse
from contextlib import contextmanager
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


def relative_to(path, parent):
    try:
        return path.relative_to(parent)
    except ValueError:
        return None


@contextmanager
def relocated(paths):
    # Move only explicitly supplied job-owned roots, once each. Moving an
    # in-source tree also relocates its nested build/install directories.
    roots = []
    for path in sorted(set(paths), key=lambda p: len(p.parts)):
        if not any(relative_to(path, root) is not None for root in roots):
            roots.append(path)
    moves = []
    try:
        for root in roots:
            destination = root.with_name(root.name + '.runtime-headers-moved')
            if destination.exists():
                raise RuntimeError('relocation destination already exists: ' + str(destination))
            root.rename(destination)
            moves.append((root, destination))

        def current(path):
            for old, new in moves:
                suffix = relative_to(path, old)
                if suffix is not None:
                    return new / suffix
            raise RuntimeError('path was not relocated: ' + str(path))

        yield current
    finally:
        for old, new in reversed(moves):
            new.rename(old)


def run(command, cwd, environment):
    result = subprocess.run(command, cwd=cwd, env=environment,
                            text=True, capture_output=True, timeout=120)
    if result.returncode != 0:
        raise RuntimeError('command failed: ' + ' '.join(map(str, command))
                           + '\n' + result.stdout + result.stderr)
    return result.stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source-dir', required=True, type=Path)
    parser.add_argument('--build-dir', required=True, type=Path)
    parser.add_argument('--install-prefix', type=Path)
    parser.add_argument('--install-bindir', default='bin')
    parser.add_argument('--relocate', action='store_true')
    parser.add_argument('--kokkos-dir', type=Path)
    parser.add_argument('--compiler-wrapper', type=Path,
                        help='optional command accepting the real compiler path followed by its arguments')
    args = parser.parse_args()
    source = args.source_dir.resolve()
    build = args.build_dir.resolve()
    install = args.install_prefix.resolve() if args.install_prefix else None
    wrapper = args.compiler_wrapper.resolve() if args.compiler_wrapper else None
    environment = os.environ.copy()
    for name in ('LFORTRAN_RUNTIME_LIBRARY_DIR', 'LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR',
                 'CPATH', 'C_INCLUDE_PATH', 'CPLUS_INCLUDE_PATH'):
        environment.pop(name, None)
    kokkos = args.kokkos_dir or environment.get('LFORTRAN_KOKKOS_DIR') or environment.get('CONDA_PREFIX')
    if kokkos:
        environment['LFORTRAN_KOKKOS_DIR'] = str(Path(kokkos).resolve())

    executable = build / 'src/bin/lfortran'
    ctest_executable = build / 'src/lfortran/tests/lfortran_runtime_headers_test'
    if not executable.is_file() or not source.is_dir():
        raise RuntimeError('source directory and fully built compiler are required')
    if ctest_executable.exists():
        raise RuntimeError('test executable already exists: ' + str(ctest_executable))
    if any(path == Path('/') or path == Path.home() for path in (source, build, install) if path):
        raise RuntimeError('refusing to relocate a filesystem or home root')
    ctest_executable.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(executable, ctest_executable)
    try:
        with tempfile.TemporaryDirectory(prefix='lfortran-runtime-headers-') as temp:
            scratch = Path(temp)
            fixture = scratch / 'runtime_headers.f90'
            fixture.write_text('''program runtime_headers
    implicit none
    integer :: value
    value = 5
    if (value * value /= 25) error stop
    print *, value * value
end program runtime_headers
''')
            def compile_with(compiler, arguments):
                command = [str(wrapper), str(compiler)] if wrapper else [str(compiler)]
                return run(command + list(map(str, arguments)), scratch, environment)

            def probe(compiler, label):
                program = scratch / (label + '-c')
                compile_with(compiler, ['--backend=c', fixture, '-o', program])
                if run([str(program)], scratch, environment).strip() != '25':
                    raise RuntimeError(label + ': C executable returned the wrong result')
                object_file = scratch / (label + '-cpp.o')
                compile_with(compiler, ['--backend=cpp', '--openmp', '-c', fixture, '-o', object_file])
                if not object_file.is_file() or object_file.stat().st_size == 0:
                    raise RuntimeError(label + ': C++ compilation did not produce an object')
                include_dir = Path(compile_with(compiler, ['--print-c-include-dir']).strip())
                if not (include_dir / 'ISO_Fortran_binding.h').is_file():
                    raise RuntimeError(label + ': ISO_Fortran_binding.h absent from printed include directory')
                print('PASS ' + label + ': C result 25, C++ object, ISO C binding header')

            def probe_all(current, label):
                tree = current(build)
                probe(tree / 'src/bin/lfortran', label + '-development')
                probe(tree / 'src/lfortran/tests/lfortran_runtime_headers_test', label + '-ctest')
                if install:
                    probe(current(install) / args.install_bindir / 'lfortran', label + '-installed')

            probe_all(lambda path: path, 'original')
            if args.relocate:
                with relocated([source, build] + ([install] if install else [])) as current:
                    if source.exists() or build.exists() or (install and install.exists()):
                        raise RuntimeError('original source/build/install paths must be unavailable')
                    # No configure or build operation occurs after the move.
                    probe_all(current, 'relocated')
    finally:
        ctest_executable.unlink()


if __name__ == '__main__':
    main()
