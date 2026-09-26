! A module array of a derived type, without an initializer of its own, takes
! the default values of the components, as a module scalar of that type does.
module derived_types_211_types
implicit none
type :: t
    integer :: z = 7
    real :: r = 1.5
end type
type :: inner
    integer :: a = 3
    real(8) :: d = 2.5d0
end type
type :: outer
    integer(8) :: k = -4_8
    type(inner) :: in
    type(inner) :: in2 = inner(10, 1.0d0)
    logical :: l = .true.
    complex :: c = (1.0, -2.0)
    integer :: nodef
end type
type, extends(outer) :: child
    integer(2) :: e = 11_2
end type
type :: ptrs
    integer, pointer :: p => null()
    integer :: n = 6
end type
end module

module derived_types_211_vars
use derived_types_211_types
implicit none
type(t) :: arr(2)
type(t) :: scalar
type(outer) :: o2(2, 3)
type(child) :: ch(4)
type(ptrs) :: pp(2)
type(outer) :: os
end module

program derived_types_211
use derived_types_211_vars
implicit none
integer :: i, j
print *, scalar%z, scalar%r, arr(1)%z, arr(1)%r
if (scalar%z /= 7) error stop 1
if (scalar%r /= 1.5) error stop 2
do i = 1, 2
    if (arr(i)%z /= 7) error stop 3
    if (arr(i)%r /= 1.5) error stop 4
end do
do j = 1, 3
    do i = 1, 2
        if (o2(i, j)%k /= -4_8) error stop 5
        if (o2(i, j)%in%a /= 3) error stop 6
        if (o2(i, j)%in%d /= 2.5d0) error stop 7
        if (o2(i, j)%in2%a /= 10) error stop 8
        if (o2(i, j)%in2%d /= 1.0d0) error stop 9
        if (.not. o2(i, j)%l) error stop 10
        if (o2(i, j)%c /= (1.0, -2.0)) error stop 11
    end do
end do
do i = 1, 4
    if (ch(i)%k /= -4_8) error stop 12
    if (ch(i)%in%a /= 3) error stop 13
    if (ch(i)%in2%a /= 10) error stop 14
    if (ch(i)%e /= 11_2) error stop 15
end do
do i = 1, 2
    if (associated(pp(i)%p)) error stop 16
    if (pp(i)%n /= 6) error stop 17
end do
if (os%k /= -4_8 .or. os%in%a /= 3 .or. os%in2%a /= 10) error stop 18
arr(1)%z = 99
o2(1, 1)%in%a = 99
if (arr(2)%z /= 7) error stop 19
if (o2(2, 1)%in%a /= 3) error stop 20
print *, "ok"
end program
