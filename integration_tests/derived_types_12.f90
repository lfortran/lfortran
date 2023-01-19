module mod
implicit none

type, public :: t
   integer :: i
contains
   generic, public :: assignment(=) => set
   procedure, private :: set

   generic, public :: operator(>) => greater
   procedure, private :: greater
end type t

contains

subroutine set(x, y)
   class(t), intent(out) :: x
   type(t), intent(in) :: y
   x%i = y%i + 1
end subroutine set

function greater(x, y) result(is_greater)
   class(t), intent(in) :: x
   type(t), intent(in) :: y
   logical :: is_greater
   is_greater = x%i > y%i
end function greater

end module mod

program alloc_assign
use mod
implicit none

logical :: is_greater
type(t) :: xt, yt
xt = t(5)
yt = xt
print *, xt%i, yt%i
if( xt%i /= 6 ) error stop
if( yt%i /= 7 ) error stop

is_greater = (xt > yt)
if (is_greater) error stop

end program alloc_assign
