! Compile-time elemental real() of an integer array with kind 16.
! The folded result cannot be packed into an ArrayConstant.
program elemental_30
    implicit none
    integer, parameter :: wp = selected_real_kind(30)
    integer :: i
    real(wp) :: x(4)
    real(wp) :: y(4)
    do i = 1, 1
        select case (i)
        case (1)
            x = real([1, 2, 3, 4], wp)
            y = real([4, 4, 4, 4], wp)
        end select
    end do
    if (abs(x(1) - 1.0_wp) > 0.0_wp) error stop 1
    if (abs(x(2) - 2.0_wp) > 0.0_wp) error stop 2
    if (abs(x(3) - 3.0_wp) > 0.0_wp) error stop 3
    if (abs(x(4) - 4.0_wp) > 0.0_wp) error stop 4
    if (any(abs(y - 4.0_wp) > 0.0_wp)) error stop 5
end program
