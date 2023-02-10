module mod
implicit none

type, public :: t
   integer :: i
contains
   generic, public :: assignment(=) => set
   procedure, private :: set

   generic, public :: operator(>) => greater
   procedure, private :: greater

   generic, public :: operator(.mul.) => multiply
   procedure, private :: multiply
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

function multiply(x, y) result(product)
   class(t), intent(in) :: x
   type(t), intent(in) :: y
   integer :: product
   product = x%i * y%i
end function multiply

end module mod

program alloc_assign
use mod
implicit none

logical :: is_greater
integer :: product
type(t) :: xt, yt
xt = t(5)
yt = xt
print *, xt%i, yt%i
if( xt%i /= 6 ) error stop
if( yt%i /= 7 ) error stop

is_greater = (xt > yt)
if (is_greater) error stop

product = (xt.mul.yt)
if (product /= 42) error stop

end program alloc_assign
