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

# Four rows per iteration: an iteration runs on the 64 lanes of a block of
# its own, which read a vector of words of each row and the four vectors of
# `x` the rows share, 8 vectors, which need the registers of 14 blocks of 64
# threads per multiprocessor.
SOURCE_ROWS = """\
module gpu_launch_bounds_rows_m
implicit none
contains
    pure real function dot4(w, x0, x1, x2, x3)
        integer, intent(in) :: w
        real, intent(in) :: x0, x1, x2, x3
        dot4 = iand(w, 255)*x0 + iand(shiftr(w, 8), 255)*x1 &
            + iand(shiftr(w, 16), 255)*x2 + shiftr(w, 24)*x3
    end function

    pure real function qdot(w, x, n, c)
        integer, intent(in) :: w(:, :)
        real, intent(in) :: x(:)
        integer, intent(in) :: n, c
        qdot = (dot4(w(4*c-3, n), x(16*c-15), x(16*c-14), x(16*c-13), x(16*c-12)) &
            + dot4(w(4*c-2, n), x(16*c-11), x(16*c-10), x(16*c-9), x(16*c-8))) &
            + (dot4(w(4*c-1, n), x(16*c-7), x(16*c-6), x(16*c-5), x(16*c-4)) &
            + dot4(w(4*c, n), x(16*c-3), x(16*c-2), x(16*c-1), x(16*c)))
    end function

    subroutine gemv(w, x, y)
        integer, intent(in) :: w(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y)/4)
            block
                real :: a1, a2, a3, a4
                integer :: c, n
                a1 = 0; a2 = 0; a3 = 0; a4 = 0
                n = 4*i - 3
                do c = 1, size(w, 1)/4
                    a1 = a1 + qdot(w, x, n, c)
                    a2 = a2 + qdot(w, x, n + 1, c)
                    a3 = a3 + qdot(w, x, n + 2, c)
                    a4 = a4 + qdot(w, x, n + 3, c)
                end do
                y(n) = a1; y(n + 1) = a2; y(n + 2) = a3; y(n + 3) = a4
            end block
        end do
    end subroutine
end module

program p
use gpu_launch_bounds_rows_m
implicit none
integer :: w(1280, 4096)
real :: x(5120), y(4096)
w = 1
x = 1
call gemv(w, x, y)
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
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "rows.f90"
        source.write_text(SOURCE_ROWS)
        out = kernel_source(sys.argv[1], source)
    # Fourteen blocks of 64 threads per multiprocessor, with the lanes
    # named as a constant.
    assert "__global__ void __launch_bounds__(64, 14)" in out, out
    assert "__gpu_lanes = 64;" in out, out
    print("ok")


main()
