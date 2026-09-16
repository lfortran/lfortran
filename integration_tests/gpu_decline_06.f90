! gpu decline reason: StructMemberTypeWidth
! a derived-type kernel argument has a numeric member of a kind the device has
! no scalar type of the same width for.
program gpu_decline_06
    implicit none
    type :: t
        real :: x
        integer(2) :: n
    end type
    type(t) :: s(4)
    integer :: i
    do i = 1, 4
        s(i)%x = 0.0
        s(i)%n = 1_2
    end do
    do concurrent (i = 1:4)
        s(i)%x = real(i)
    end do
    do i = 1, 4
        if (abs(s(i)%x - real(i)) > 1.0e-6) error stop "bad x"
        if (s(i)%n /= 1_2) error stop "bad n"
    end do
end program
