! A use-associated module subroutine passed as an actual argument to an
! `external` procedure defined later in the same file (compiled with
! --std=f23, which enables implicit argument casting).
module external_25_mod
implicit none
contains
subroutine incr(x)
integer, intent(inout) :: x
x = x + 1
end subroutine
end module

program external_25
use external_25_mod, only: incr
implicit none
external apply_sub
integer :: i
i = 1
call apply_sub(incr, i)
if (i /= 2) error stop
print *, i
end program

subroutine apply_sub(g, i)
external g
integer :: i
call g(i)
end subroutine
