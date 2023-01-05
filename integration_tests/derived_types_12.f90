module mod
implicit none

type, public :: t
   integer :: i
contains
   generic, public :: assignment(=) => set
   procedure, private :: set
end type t

contains

subroutine set(x, y)
   class(t), intent(out) :: x
   type(t), intent(in) :: y
   x%i = y%i + 1
end subroutine set

end module mod

program alloc_assign
use mod
implicit none

type(t) :: xt, yt
xt = t(5)
yt = xt
print *, xt%i, yt%i
if( xt%i /= 6 ) error stop
if( yt%i /= 7 ) error stop

end program alloc_assign
