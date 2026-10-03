! Use-associated module procedures passed as actual arguments to an
! `external` procedure that has no explicit interface.
module external_24_mod
implicit none
contains
subroutine incr(x)
integer, intent(inout) :: x
x = x + 1
end subroutine

integer function twice(x)
integer, intent(in) :: x
twice = 2*x
end function
end module

program external_24
use external_24_mod, only: incr, twice
implicit none
external apply_sub, apply_fun
integer :: i
i = 1
call apply_sub(incr, i)
if (i /= 2) error stop
call apply_sub(incr, i)
if (i /= 3) error stop
call apply_fun(twice, i)
if (i /= 6) error stop
print *, i
end program

subroutine apply_sub(g, i)
external g
integer :: i
call g(i)
end subroutine

subroutine apply_fun(h, i)
integer, external :: h
integer :: i
i = h(i)
end subroutine
