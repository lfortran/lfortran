program array_128
    implicit none
    integer, parameter :: a_lo = -1
    real :: ap(a_lo:a_lo, 1:1)
    real :: bp(1, 1)
    real :: cp(1, 1)

    if (a_lo < 1) then
        error stop "Error: Array bounds are invalid for matmul."
    end if

    ap = 1.0
    bp = 2.0
    cp = matmul(ap, bp)
    print *, cp(1, 1)
end program array_128