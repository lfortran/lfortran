! Iterative radix-2 complex FFT, checked by a forward + inverse round trip.
module fft_radix2_mod
implicit none
integer, parameter :: dp = kind(0.d0)

contains

    subroutine fft(x, sgn)
    complex(dp), intent(inout) :: x(0:)
    integer, intent(in) :: sgn
    real(dp), parameter :: pi = 4 * atan(1.0_dp)
    complex(dp) :: w, t
    integer :: n, i, j, k, m, half
    n = size(x)
    ! bit-reversal permutation
    j = 0
    do i = 0, n - 2
        if (i < j) then
            t = x(i)
            x(i) = x(j)
            x(j) = t
        end if
        k = n / 2
        do while (k <= j)
            j = j - k
            k = k / 2
        end do
        j = j + k
    end do
    m = 2
    do while (m <= n)
        half = m / 2
        do j = 0, half - 1
            w = cmplx(cos(2*pi*j/m), sgn * sin(2*pi*j/m), dp)
            do k = 0, n - 1, m
                t = w * x(k + j + half)
                x(k + j + half) = x(k + j) - t
                x(k + j) = x(k + j) + t
            end do
        end do
        m = 2 * m
    end do
    end subroutine

end module

program fft_radix2
use fft_radix2_mod, only: dp, fft
implicit none
integer, parameter :: n = 2**18, nrep = 20
complex(dp), allocatable :: x(:), x0(:)
real(dp) :: err
integer :: i, r

allocate(x(0:n-1), x0(0:n-1))
do i = 0, n - 1
    x0(i) = cmplx(sin(0.001_dp * i), cos(0.002_dp * i), dp)
end do
x = x0

do r = 1, nrep
    call fft(x, -1)
    call fft(x, 1)
    x = x / n
end do

err = maxval(abs(x - x0))
print *, "checksum:", sum(real(x, dp)), "max error:", err
if (err > 1e-10_dp) error stop
end program
