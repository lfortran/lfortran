! An intrinsic assignment to an allocatable scalar of derived type allocates
! the variable when it is unallocated (F2018 10.2.1.3 p3), for a variable,
! a component and a polymorphic value, and keeps an allocated one.
module derived_types_234_m
    implicit none
    type :: point
        integer :: x = 0
        integer, allocatable :: w(:)
    end type

    type, extends(point) :: point3
        integer :: z = 0
    end type

    type :: box
        type(point), allocatable :: p
    end type
contains
    subroutine assign_from_class(a, c)
        type(point), allocatable, intent(inout) :: a
        class(point), intent(in) :: c
        a = c
    end subroutine
end module

program derived_types_234
    use derived_types_234_m
    implicit none
    type(point), allocatable :: a
    type(point) :: b
    type(box) :: bx
    type(point3) :: q

    b%x = 1
    b%w = [1, 2, 3]
    a = b
    if (.not. allocated(a)) error stop 1
    if (a%x /= 1 .or. size(a%w) /= 3) error stop 2
    b%x = 2
    b%w = [4, 5]
    a = b
    if (a%x /= 2 .or. any(a%w /= [4, 5])) error stop 3
    a = point(a%x + 1, [a%w, 6])
    if (a%x /= 3 .or. any(a%w /= [4, 5, 6])) error stop 4

    bx%p = b
    if (.not. allocated(bx%p)) error stop 5
    if (bx%p%x /= 2) error stop 6

    deallocate(a)
    q%x = 7
    q%z = 8
    call assign_from_class(a, q)
    if (.not. allocated(a)) error stop 7
    if (a%x /= 7 .or. allocated(a%w)) error stop 8
    print *, "ok"
end program
