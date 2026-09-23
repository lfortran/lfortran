module derived_types_191_mod
implicit none

type :: leaf
    integer :: v = 0
end type leaf

type :: twig
    type(leaf) :: l = leaf(0)
    real :: r = 0.0
end type twig

type :: bunch
    type(leaf) :: a(2)
    type(twig) :: t(2)
    integer :: h = 0
end type bunch

end module derived_types_191_mod


program derived_types_191
use derived_types_191_mod
implicit none

type(bunch) :: b

! An array constructor of a derived type is lowered into a temporary of that
! type, and that temporary is declared by the type's symbol
b = bunch(a=[leaf(1), leaf(2)], t=[twig(leaf(3), 1.5), twig(leaf(4), 2.5)], h=5)
call check(b, 1, 2, 3, 1.5, 4, 2.5, 5)

call in_a_procedure()

contains

    subroutine in_a_procedure()
    type(bunch) :: c
    c = bunch(a=[leaf(11), leaf(12)], &
              t=[twig(leaf(13), 3.5), twig(leaf(14), 4.5)], h=15)
    call check(c, 11, 12, 13, 3.5, 14, 4.5, 15)
    end subroutine in_a_procedure

    subroutine check(x, a1, a2, t1, r1, t2, r2, hv)
    type(bunch), intent(in) :: x
    integer, intent(in) :: a1, a2, t1, t2, hv
    real, intent(in) :: r1, r2
    if (x%a(1)%v /= a1) error stop "a(1)%v"
    if (x%a(2)%v /= a2) error stop "a(2)%v"
    if (x%t(1)%l%v /= t1) error stop "t(1)%l%v"
    if (x%t(1)%r /= r1) error stop "t(1)%r"
    if (x%t(2)%l%v /= t2) error stop "t(2)%l%v"
    if (x%t(2)%r /= r2) error stop "t(2)%r"
    if (x%h /= hv) error stop "h"
    end subroutine check

end program derived_types_191
