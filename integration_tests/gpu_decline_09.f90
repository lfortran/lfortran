! gpu decline reason: ScalarNotNumeric
! a scalar kernel argument is not an integer, a real or a logical, so the
! device layout has no scalar type to pass it by value in.
program gpu_decline_09
    implicit none
    complex :: c
    real :: a(4)
    integer :: i
    c = (2.0, 1.0)
    do concurrent (i = 1:4)
        a(i) = real(i) * real(c)
    end do
    do i = 1, 4
        if (abs(a(i) - 2.0 * real(i)) > 1.0e-6) error stop "bad a"
    end do
end program
