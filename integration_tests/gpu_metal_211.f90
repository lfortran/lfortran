! An ASSOCIATE whose selector is an array-valued STRUCTURE CONSTRUCTOR
! resolved through a generic interface. The compiler materializes the
! selector into a temporary owned by the ASSOCIATE block, and the
! defining expression references the resolved specific procedure through
! a symbol that is only reachable from the ASSOCIATE block's scope. A
! `do concurrent` inside the block must take the materialized temporary
! as a kernel argument rather than inline the constructor call.
module gpu_metal_211_mod
implicit none

type :: interp_t
    integer :: order_
    real :: dx_
contains
    procedure :: at
end type

interface interp_t
    elemental module function ctor(order, dx) result(r)
        integer, intent(in) :: order
        real, intent(in) :: dx
        type(interp_t) :: r
    end function
end interface

interface
    elemental module function at(self, x) result(y)
        class(interp_t), intent(in) :: self
        real, intent(in) :: x
        real :: y
    end function
end interface

end module

submodule (gpu_metal_211_mod) gpu_metal_211_smod
contains
    module procedure ctor
        r%order_ = order
        r%dx_ = dx
    end procedure
    module procedure at
        y = self%dx_ * x + real(self%order_)
    end procedure
end submodule

program gpu_metal_211
use gpu_metal_211_mod
implicit none
real :: a(4), b(4)
integer :: i

a = [1.0, 2.0, 3.0, 4.0]
b = 0.0

associate(interp => interp_t(order=[1,2,3], dx=[0.5,0.25,0.125]))
    do concurrent (integer :: j = 1:4)
        b(j) = interp(2)%at(a(j))
    end do
end associate
do i = 1, 4
    if (abs(b(i) - (0.25 * a(i) + 2.0)) > 1.0e-5) error stop "generic constructor selector"
end do

b = 0.0
associate(interp => interp_t(order=[1,2,3], dx=[0.5,0.25,0.125]))
    do concurrent (integer :: j = 1:4)
        b(j) = interp(1)%at(a(j)) + interp(3)%dx_
    end do
end associate
do i = 1, 4
    if (abs(b(i) - (0.5 * a(i) + 1.0 + 0.125)) > 1.0e-5) error stop "two uses of constructor selector"
end do

print *, "ok"
end program
