import subprocess
import sys
import tempfile
from pathlib import Path

# A sum over `dk` elements read down the columns of a matrix, `dk` known only
# at run time.
SHORT = """\
program p
implicit none
integer :: dk, rows, j, k
real, allocatable :: w(:, :), y(:)
dk = 128 + command_argument_count()*0
rows = 3000
allocate(w(dk, rows), y(rows))
w = 1
do concurrent (j = 1:rows)
    block
        real :: acc
        integer :: n
        acc = 0
        do n = 1, dk
            acc = acc + w(n, j)
        end do
        y(j) = acc
    end block
end do
print *, y(1)
end program
"""

# The same, over the number of elements that an array holds: not a parameter
# of the kernel, so not known when the kernel is launched.
UNKNOWN = SHORT.replace("do n = 1, dk", "do n = 1, nint(w(1, 1))*dk")


def llvm_ir(lfortran, source):
    return subprocess.run(
        [lfortran, "--gpu=cuda", "--show-llvm", str(source)],
        check=True, capture_output=True, text=True).stdout


def main():
    lfortran = sys.argv[1]
    with tempfile.TemporaryDirectory() as tmp:
        short = Path(tmp) / "short.f90"
        short.write_text(SHORT)
        unknown = Path(tmp) / "unknown.f90"
        unknown.write_text(UNKNOWN)
        # The launch of a kernel whose loops are all shorter than 8 * 256
        # iterations (8 per thread of a block) compares the length of the
        # loop with 2048, to keep the lanes of a warp rather than a block.
        host = llvm_ir(lfortran, short)
        assert " 2048\n" in host, "no test of the length of the loop"
        # A kernel with a loop of any other length keeps what it had.
        host = llvm_ir(lfortran, unknown)
        assert " 2048\n" not in host, "test of an unknown length"
    print("ok")


main()
