module derived_types_189_mod
use iso_c_binding, only: c_ptr, c_null_ptr
implicit none

type :: leaf
    integer :: v = 0
end type leaf

type :: node
    type(leaf) :: l = leaf(0)
    real :: r = 0.0
end type node

type :: box
    type(leaf) :: a(2)
    type(leaf) :: b(2,3)
    type(node) :: n(2)
    integer :: g(2) = 0
    type(c_ptr) :: cp(2)
    integer :: h = 0
end type box

! A component with no elements has nothing to spread
type :: empty_box
    type(leaf) :: e(0)
    integer :: h = 0
end type empty_box

! A type with an allocatable component is not stored as a constant, so a
! scalar constructor of it is assigned to every element instead
type :: holder
    integer, allocatable :: p(:)
    integer :: v = 0
end type holder

type :: holder_box
    type(holder) :: a(2)
    integer :: h = 0
end type holder_box

! A component default is a static initializer too
type :: wrapper
    type(box) :: w = box(a=leaf(41), b=leaf(42), n=node(leaf(43), 4.5), &
                         g=44, cp=c_null_ptr, h=45)
end type wrapper

! A module variable is initialized statically
type(box) :: mb = box(a=leaf(31), b=leaf(32), n=node(leaf(33), 3.5), &
                      g=34, cp=c_null_ptr, h=35)

end module derived_types_189_mod


program derived_types_189
use iso_c_binding, only: c_null_ptr, c_associated
use derived_types_189_mod
implicit none

! A scalar structure constructor for an array component of derived type is
! spread over every element of that component, as a scalar of an intrinsic
! type is
type(box), save :: b1 = box(a=leaf(5), b=leaf(6), n=node(leaf(8), 1.5), &
                            g=9, cp=c_null_ptr, h=1)
! An array constructor argument keeps its own elements
type(box), save :: b2 = box(a=[leaf(11), leaf(12)], b=leaf(13), &
                            n=node(leaf(14), 2.5), g=15, cp=c_null_ptr, h=16)
type(box) :: b3
type(wrapper) :: wr
type(empty_box), save :: eb = empty_box(e=leaf(51), h=52)
type(empty_box) :: eb2
type(holder_box) :: hb
type(holder_box) :: hb2

call check_spread(b1, 5, 6, 8, 1.5, 9, 1, "b1")
call check_spread(mb, 31, 32, 33, 3.5, 34, 35, "mb")
call check_spread(wr%w, 41, 42, 43, 4.5, 44, 45, "wr%w")

! The same constructor in an assignment
b3 = box(a=leaf(21), b=leaf(22), n=node(leaf(23), 2.5), g=24, cp=c_null_ptr, h=25)
call check_spread(b3, 21, 22, 23, 2.5, 24, 25, "b3")

if (b2%a(1)%v /= 11) error stop "b2%a(1)%v"
if (b2%a(2)%v /= 12) error stop "b2%a(2)%v"
if (b2%b(1,1)%v /= 13) error stop "b2%b(1,1)%v"
if (b2%b(2,3)%v /= 13) error stop "b2%b(2,3)%v"
if (b2%n(2)%l%v /= 14) error stop "b2%n(2)%l%v"
if (b2%g(2) /= 15) error stop "b2%g(2)"
if (b2%h /= 16) error stop "b2%h"

if (size(eb%e) /= 0) error stop "eb%e size"
if (eb%h /= 52) error stop "eb%h"

! The same empty component in an assignment: the other components survive
eb2%h = -1
eb2 = empty_box(e=leaf(61), h=62)
if (size(eb2%e) /= 0) error stop "eb2%e size"
if (eb2%h /= 62) error stop "eb2%h"

! An element type with an allocatable component
hb = holder_box(a=holder(p=[1, 2], v=71), h=72)
if (hb%a(1)%v /= 71) error stop "hb%a(1)%v"
if (hb%a(2)%v /= 71) error stop "hb%a(2)%v"
if (size(hb%a(1)%p) /= 2) error stop "hb%a(1)%p size"
if (size(hb%a(2)%p) /= 2) error stop "hb%a(2)%p size"
if (hb%a(1)%p(1) /= 1) error stop "hb%a(1)%p(1)"
if (hb%a(1)%p(2) /= 2) error stop "hb%a(1)%p(2)"
if (hb%a(2)%p(1) /= 1) error stop "hb%a(2)%p(1)"
if (hb%a(2)%p(2) /= 2) error stop "hb%a(2)%p(2)"
if (hb%h /= 72) error stop "hb%h"

! The same, with the allocatable component left unallocated
hb2 = holder_box(a=holder(v=81), h=82)
if (hb2%a(1)%v /= 81) error stop "hb2%a(1)%v"
if (hb2%a(2)%v /= 81) error stop "hb2%a(2)%v"
if (allocated(hb2%a(1)%p)) error stop "hb2%a(1)%p allocated"
if (allocated(hb2%a(2)%p)) error stop "hb2%a(2)%p allocated"
if (hb2%h /= 82) error stop "hb2%h"

contains

    subroutine check_spread(x, av, bv, nv, nr, gv, hv, tag)
    type(box), intent(in) :: x
    integer, intent(in) :: av, bv, nv, gv, hv
    real, intent(in) :: nr
    character(len=*), intent(in) :: tag
    integer :: i, j
    do i = 1, 2
        if (x%a(i)%v /= av) error stop "a not spread in " // tag
        ! A nested constructor is spread with all of its components
        if (x%n(i)%l%v /= nv) error stop "n%l not spread in " // tag
        if (x%n(i)%r /= nr) error stop "n%r not spread in " // tag
        ! An intrinsic scalar keeps working
        if (x%g(i) /= gv) error stop "g not spread in " // tag
        if (c_associated(x%cp(i))) error stop "cp not spread in " // tag
        ! A rank-2 component is spread over every element
        do j = 1, 3
            if (x%b(i,j)%v /= bv) error stop "b not spread in " // tag
        end do
    end do
    if (x%h /= hv) error stop "h in " // tag
    end subroutine check_spread

end program derived_types_189
