! gpu decline reason: StructMemberNotNumeric
! a derived-type kernel argument has a member that is not an integer, a real
! or a logical, so the device layout has no scalar type for it.
program gpu_decline_07
    implicit none
    type :: t
        real :: x
        complex :: z
    end type
    type(t) :: s(4)
    integer :: i
    do i = 1, 4
        s(i)%x = 0.0
        s(i)%z = (1.0, 0.0)
    end do
    do concurrent (i = 1:4)
        s(i)%x = real(i)
    end do
    do i = 1, 4
        if (abs(s(i)%x - real(i)) > 1.0e-6) error stop "bad x"
    end do
end program
