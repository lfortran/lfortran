module class_allocate_10_mod
    implicit none

    type :: base
        real :: x = 5
        integer :: tag = -1
    end type

    type, extends(base) :: circle
        real :: r = 1
        character(len=4) :: label = 'circ'
        integer, allocatable :: hist(:)
    end type

    type, extends(circle) :: ring
        real :: w = 0.25
    end type

    type :: holder
        class(base), allocatable :: items(:)
    end type

end module

program class_allocate_10
use class_allocate_10_mod
implicit none
class(base), allocatable :: a(:), b(:,:)
class(*), allocatable :: u(:)
type(holder) :: h
integer :: i

! Components of the type-spec's type get their default values
allocate(circle :: a(3))
select type (a)
type is (circle)
    do i = 1, 3
        if (abs(a(i)%x - 5.0) > 1e-6) error stop
        if (a(i)%tag /= -1) error stop
        if (abs(a(i)%r - 1.0) > 1e-6) error stop
        if (a(i)%label /= 'circ') error stop
        if (allocated(a(i)%hist)) error stop
    end do
    print *, a(3)%x, a(3)%tag, a(3)%r, a(3)%label
class default
    error stop
end select
deallocate(a)

! Two levels of extension
allocate(ring :: a(4))
select type (a)
type is (ring)
    do i = 1, 4
        if (abs(a(i)%x - 5.0) > 1e-6) error stop
        if (abs(a(i)%r - 1.0) > 1e-6) error stop
        if (abs(a(i)%w - 0.25) > 1e-6) error stop
    end do
    print *, a(4)%x, a(4)%r, a(4)%w
class default
    error stop
end select
deallocate(a)

! The declared type as type-spec keeps working
allocate(base :: a(2))
if (abs(a(2)%x - 5.0) > 1e-6) error stop
if (a(2)%tag /= -1) error stop
print *, a(2)%x, a(2)%tag

! Rank 2
allocate(circle :: b(2, 3))
select type (b)
type is (circle)
    if (abs(b(2, 3)%r - 1.0) > 1e-6) error stop
    if (b(1, 2)%label /= 'circ') error stop
    if (abs(b(2, 1)%x - 5.0) > 1e-6) error stop
    print *, b(2, 3)%r, b(1, 2)%label
class default
    error stop
end select

! Unlimited polymorphic
allocate(ring :: u(2))
select type (u)
type is (ring)
    if (abs(u(1)%x - 5.0) > 1e-6) error stop
    if (abs(u(2)%r - 1.0) > 1e-6) error stop
    if (abs(u(2)%w - 0.25) > 1e-6) error stop
    if (u(2)%label /= 'circ') error stop
    print *, u(2)%x, u(2)%r, u(2)%w
class default
    error stop
end select

! Polymorphic array component
allocate(circle :: h%items(2))
select type (items => h%items)
type is (circle)
    if (abs(items(2)%r - 1.0) > 1e-6) error stop
    if (items(1)%tag /= -1) error stop
    print *, items(2)%r, items(1)%tag
class default
    error stop
end select

end program class_allocate_10
