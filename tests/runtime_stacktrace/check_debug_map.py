"""Exercise debug-map generation and runtime lookup through the compiler CLI."""
import argparse
import pathlib
import re
import struct
import subprocess
import tempfile


def run(command, cwd):
    return subprocess.run(command, cwd=cwd, capture_output=True, text=True,
                          timeout=60)


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def compile_program(compiler, source, output, work):
    result = run([compiler, '-g', '--no-color', str(source), '-o', output], work)
    require(result.returncode == 0, result.stdout + result.stderr)
    return result


def check_frames(executable, work):
    result = run([str(executable)], work)
    require(result.returncode == 1, result.stdout + result.stderr)
    frames = [int(n) for n in re.findall(r'  File .*?, line (\d+)', result.stderr)]
    require(frames == [3, 10, 20, 14], result.stderr)
    require(result.stdout == '', result.stdout)


def check_raw(executable, work):
    result = run([str(executable)], work)
    require(result.returncode == 1, result.stdout + result.stderr)
    require('printing raw addresses' in result.stderr, result.stderr)
    require('  File ' not in result.stderr, result.stderr)
    require(re.search(r'  0x[0-9a-f]+', result.stderr), result.stderr)


def check_paths(compiler, source, work):
    (work / 'out.dir').mkdir()
    for name in ('plain', './relative', 'out.dir/program',
                 'out.dir/program.exe', '.hidden', '.hidden.exe'):
        compile_program(compiler, source, name, work)
        check_frames((work / name).resolve(), work)
        print('PASS output path', name)


def check_failures(compiler, source, work):
    compile_program(compiler, source, 'program', work)
    executable = work / 'program'
    map_path = work / 'program_lines.dat'
    original = map_path.read_bytes()
    map_path.unlink()
    check_raw(executable, work)
    for payload in (b'', b'\x00', b'\x00' * 23,
                    struct.pack('=QQQ', 0, 0, 0) * 250):
        map_path.write_bytes(payload)
        check_raw(executable, work)
        print('PASS unavailable map of', len(payload), 'bytes')
    map_path.write_bytes(original + b'\x00' * 23)
    check_frames(executable, work)
    for attempt in range(3):
        map_path.write_bytes(b'stale previous map')
        compile_program(compiler, source, 'program', work)
        check_frames(executable, work)
        print('PASS repeated debug link', attempt + 1)
    if pathlib.Path('/dev/full').exists():
        map_path.unlink()
        map_path.symlink_to('/dev/full')
        result = compile_program(compiler, source, 'program', work)
        require('warning: could not generate' in result.stderr, result.stderr)
        require(not map_path.is_symlink(), 'failed map was not removed')
        check_raw(executable, work)
        print('PASS buffered write failure warns, removes map, and runs')


def check_nodebug(compiler, source, work):
    for name, text in (
            ('m1', 'module m1\ncontains\ninteger function f()\nf=42\n'
             'end function\nend module\n'),
            ('p1', 'program p1\nuse m1\nif(f()/=42) error stop\nend program\n')):
        (work / (name + '.f90')).write_text(text)
        result = run([compiler, '-c', name + '.f90', '-o', name + '.o'], work)
        require(result.returncode == 0, result.stdout + result.stderr)
    map_path = work / 'program_lines.dat'
    map_path.write_bytes(b'stale previous map')
    result = run([compiler, '-g', 'm1.o', 'p1.o', '-o', 'program'], work)
    require(result.returncode == 0, result.stdout + result.stderr)
    require(map_path.read_bytes() == b'', 'no-DWARF link kept stale data')
    result = run([str(work / 'program')], work)
    require(result.returncode == 0, result.stdout + result.stderr)
    print('PASS no-DWARF objects link with -g and replace the stale map')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--lfortran', required=True)
    parser.add_argument('--case', choices=('paths', 'failures', 'nodebug'),
                        required=True)
    args = parser.parse_args()
    compiler = str(pathlib.Path(args.lfortran).resolve())
    source = pathlib.Path(__file__).resolve().parents[1] / 'errors/runtime_stacktrace_01.f90'
    with tempfile.TemporaryDirectory(prefix='lfortran-debug-map-') as directory:
        globals()['check_' + args.case](compiler, source, pathlib.Path(directory))
