program internal_subr_assumed_size_01
implicit none

real :: x(4)
real :: total

x = [1.0, 2.0, 3.0, 4.0]
call sub(x, 4, total)

if (abs(total - 10.0) > 1.e-6) error stop

end program

subroutine sub(a, n, total)
implicit none

real, intent(in) :: a(*)
integer, intent(in) :: n
real, intent(out) :: total

total = 0.0
call nested()

contains

    subroutine nested()
    integer :: i

    do i = 1, n
        total = total + a(i)
    end do
    end subroutine

end subroutine
