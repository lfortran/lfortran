! gpu decline reason: LiveoutScalar
! A complex scalar the loop defines, whose value the host reads after it,
! which the buffer that carries a value back to the host cannot hold.
program gpu_decline_29
    implicit none
    complex :: z
    real :: a(100)
    integer :: i
    z = 0
    do concurrent (i = 1:100)
        a(i) = i
        if (i == 5) z = cmplx(i, -i)
    end do
    print *, z
    if (z /= cmplx(5, -5)) error stop 1
    if (a(100) /= 100) error stop 2
end program
