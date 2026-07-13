program array_128
    implicit none

    integer, parameter :: a_lo = -1

    real :: ap(a_lo:a_lo, 1:1)
    real :: bp(1, 1)
    real :: cp(1, 1)

    ap = 1.0
    bp = 2.0
  
    cp = matmul(ap, bp)

    if (abs(cp(1, 1) - 2.0) > 1e-5) then
        error stop "Error: matmul result is incorrect."
    end if

end program array_128