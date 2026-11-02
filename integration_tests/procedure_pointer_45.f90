! Procedure pointer components without an explicit interface keep their
! implicit interface when they are associated: each call through one uses
! an interface built from its actual arguments (#16414).
program procedure_pointer_45
implicit none
type :: holder
    procedure(), pointer, nopass :: s => null()
    procedure(integer), pointer, nopass :: f => null()
end type
type(holder) :: obj, objs(2)
integer :: res
obj%s => isq
call obj%s(6, res)
if (res /= 36) error stop 1
obj%s => iadd
call obj%s(6, 7, res)
if (res /= 13) error stop 2
obj%s => isq
call obj%s(3, res)
if (res /= 9) error stop 3

obj%f => inc
if (obj%f(3) /= 4) error stop 4
obj%f => sum2
if (obj%f(3, 5) /= 8) error stop 5

objs(1)%f => inc
objs(2)%f => sum2
if (objs(1)%f(objs(2)%f(1, 2)) /= 4) error stop 6
if (objs(2)%f(objs(1)%f(1), objs(1)%f(2)) /= 5) error stop 7
objs(2)%s => iadd
call objs(2)%s(1, 2, res)
if (res /= 3) error stop 8
print *, "ok"
contains
subroutine isq(n, out)
    integer, intent(in) :: n
    integer, intent(out) :: out
    out = n * n
end subroutine
subroutine iadd(a, b, out)
    integer, intent(in) :: a, b
    integer, intent(out) :: out
    out = a + b
end subroutine
integer function inc(n)
    integer, intent(in) :: n
    inc = n + 1
end function
integer function sum2(a, b)
    integer, intent(in) :: a, b
    sum2 = a + b
end function
end program
