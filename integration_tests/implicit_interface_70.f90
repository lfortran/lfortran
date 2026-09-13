! An external subroutine defined in this file is passed to a procedure defined
! in another file (implicit_interface_70b.f90), and afterwards called directly
! with an array element for its adjustable array dummy (sequence association).
program implicit_interface_70
implicit none
external scale70
real :: a(10)
integer :: i
do i = 1, 10
    a(i) = i
end do
call apply70(scale70, a)
if (abs(a(10) - 20) > 1e-5) error stop
call scale70(5, a(3))
if (abs(a(3) - 12) > 1e-5) error stop
if (abs(a(7) - 28) > 1e-5) error stop
if (abs(a(8) - 16) > 1e-5) error stop
print *, "ok"
end program

subroutine scale70(n, x)
integer :: n
real :: x(n)
x = 2*x
end subroutine
