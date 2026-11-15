import subprocess
import sys
import tempfile
from pathlib import Path

# The matrix-vector product of gpu_launch_bounds_01, whose lanes read vectors
# of 16 bytes.
SOURCE = """\
module gpu_launch_bounds_m
! A matrix-vector product of packed 8-bit words whose lanes read vectors of 16
! bytes. On an NVIDIA GPU its kernel is bound to the registers of six blocks
! of 256 threads per multiprocessor and does not unroll the loop of its lanes.
! The data is integer-valued, so that the sums are exact.
implicit none
contains
    pure function dot4(a, b, c) result(r)
        integer, intent(in) :: a, b, c
        integer :: r
        r = c + ibits(a, 0, 8)*ibits(b, 0, 8) + ibits(a, 8, 8)*ibits(b, 8, 8) &
            + ibits(a, 16, 8)*ibits(b, 16, 8) + ibits(a, 24, 8)*ibits(b, 24, 8)
    end function

    pure function chunk_idot(w, xw, n, c) result(r)
        integer, intent(in) :: w(:, :), xw(:)
        integer, intent(in) :: n, c
        integer :: r
        r = dot4(w(4*c-3, n), xw(4*c-3), dot4(w(4*c-2, n), xw(4*c-2), &
            dot4(w(4*c-1, n), xw(4*c-1), dot4(w(4*c, n), xw(4*c), 0))))
    end function

    pure subroutine qmv(xw, sx, w, y)
        integer, contiguous, intent(in) :: xw(:), w(:, :)
        real, contiguous, intent(in) :: sx(:)
        real, contiguous, intent(out) :: y(:)
        integer :: k, n

        k = 4*size(w, 1)
        do concurrent (n = 1:size(y))
            block
                real :: total
                integer :: q, c

                total = 0
                do q = 1, k/32
                    c = 2*q - 1
                    total = total + sx(c)*real(chunk_idot(w, xw, n, c))
                    total = total + sx(c+1)*real(chunk_idot(w, xw, n, c+1))
                end do
                y(n) = total
            end block
        end do
    end subroutine
end module

program p
use gpu_launch_bounds_m
implicit none
integer :: w(1280, 4099), xw(1280)
real :: sx(320), y(4099)
w = 1
xw = 1
sx = 1
call qmv(xw, sx, w, y)
print *, y(1)
end program
"""


def kernel_source(lfortran, source):
    result = subprocess.run(
        [lfortran, "--fast", "--gpu=cuda", "--show-gpu-kernel-source",
         str(source)],
        capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
    return result.stdout


def main():
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "p.f90"
        source.write_text(SOURCE)
        out = kernel_source(sys.argv[1], source)
    kernel = out[out.index("__global__"):]
    # Six blocks of 256 threads per multiprocessor.
    assert "__global__ void __launch_bounds__(256, 6)" in out, out
    # The loop of the lanes, written for aligned and for other arrays, is
    # not unrolled.
    assert "#pragma unroll 2" not in kernel, kernel
    assert kernel.count("#pragma unroll 1") >= 1, kernel
    print("ok")


main()
