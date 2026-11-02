! A procedure pointer without an explicit interface keeps its implicit
! interface when it is associated, so it can then be associated with
! procedures of other interfaces (#16414).
module procedure_pointer_44_m
implicit none
abstract interface
    subroutine callback(v)
        integer, intent(out) :: v
    end subroutine
end interface
integer :: nreset = 0
contains
subroutine get(v)
    integer, intent(out) :: v
    v = 1
end subroutine
subroutine reset()
    nreset = nreset + 1
end subroutine
subroutine add(a, b, c)
    integer, intent(in) :: a, b
    integer, intent(out) :: c
    c = a + b
end subroutine
integer function twice(a)
    integer, intent(in) :: a
    twice = 2 * a
end function
integer function sum3(a, b, c)
    integer, intent(in) :: a, b, c
    sum3 = a + b + c
end function
end module

program procedure_pointer_44
use procedure_pointer_44_m
implicit none
procedure(callback), pointer :: current
procedure(), pointer :: opaque
procedure(integer), pointer :: ifun
integer :: v
current => get
opaque => current
call opaque(v)
if (v /= 1) error stop 1
opaque => reset
call opaque()
if (nreset /= 1) error stop 2
opaque => add
call opaque(2, 3, v)
if (v /= 5) error stop 3
opaque => get
v = 0
call opaque(v)
if (v /= 1) error stop 4

ifun => twice
if (ifun(4) /= 8) error stop 5
ifun => sum3
if (ifun(1, 2, 3) /= 6) error stop 6
ifun => twice
if (ifun(5) /= 10) error stop 7
print *, "ok"
end program
