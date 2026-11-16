! scale(x, i) where 2**i overflows or underflows but x*2**i does not.
program scale_extreme_01
    implicit none
    real(8) :: x
    real(4) :: y
    integer :: k
    x = 0.75d0
    k = 1024
    print *, scale(x, k)
    if (scale(x, k) /= 1.5d0*2d0**1023) error stop "overflowing 2**i"
    k = -1075
    if (scale(4*x, k) /= tiny(1d0)*epsilon(1d0)*2) error stop "underflowing 2**i"
    y = 0.5
    k = 128
    if (scale(y, k) /= 2.0**127) error stop "real(4)"
end program
