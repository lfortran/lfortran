! Type-bound procedures of types of two modules, implemented by functions
! of one name, called in a do concurrent loop: each binding reaches its own
! procedure in device code, with and without a passed-object dummy.
module gpu_metal_591_a
implicit none
type :: ta
    real :: v
contains
    procedure, nopass :: get => get_impl
    procedure :: val => val_impl
end type
contains
pure real function get_impl(x)
    real, intent(in) :: x
    get_impl = x * 2
end function
pure real function val_impl(self)
    class(ta), intent(in) :: self
    val_impl = self%v * 2
end function
end module

module gpu_metal_591_b
implicit none
type :: tb
    real :: v
contains
    procedure, nopass :: get => get_impl
    procedure :: val => val_impl
end type
contains
pure real function get_impl(x)
    real, intent(in) :: x
    get_impl = x + 100
end function
pure real function val_impl(self)
    class(tb), intent(in) :: self
    val_impl = self%v + 100
end function
end module

program gpu_metal_591
use gpu_metal_591_a, only: ta
use gpu_metal_591_b, only: tb
implicit none
integer, parameter :: n = 100
real :: o(n), p(n)
type(ta) :: a
type(tb) :: b
integer :: i
a%v = 3
b%v = 5
do concurrent (i = 1:n)
    o(i) = a%get(real(i)) + 10*b%get(real(i))
end do
do concurrent (i = 1:n)
    p(i) = a%val() + b%val() + i
end do
print *, o(1:3), p(1:3)
do i = 1, n
    if (abs(o(i) - (2*i + 10*(i + 100))) > 1e-3) error stop 1
    if (abs(p(i) - (6 + 105 + i)) > 1e-3) error stop 2
end do
end program
