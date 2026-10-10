! Allocatable derived type actual arguments passed to an optional
! class(*) or class(base) dummy
module select_type_56_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type, extends(item_t) :: child_t
        integer :: w = 0
    end type
    type :: holder_t
        type(item_t), allocatable :: it
    end type
contains
    subroutine consumes(r, item)
        integer, intent(out) :: r
        class(*), optional :: item
        r = -1
        if (present(item)) then
            r = -2
            select type (item)
            type is (item_t)
                r = item%v
            end select
        end if
    end subroutine

    integer function get_base(item) result(r)
        class(item_t), optional :: item
        r = -1
        if (present(item)) then
            r = -2
            select type (item)
            type is (child_t)
                r = item%v + item%w
            end select
        end if
    end function
end module

program select_type_56
    use select_type_56_mod
    implicit none
    integer :: r
    type(item_t), allocatable :: a, b
    type(child_t), allocatable :: c
    type(holder_t) :: h

    allocate(a)
    a%v = 4
    call consumes(r, a)
    if (r /= 4) error stop 1
    call consumes(r, b)
    if (r /= -1) error stop 2
    call consumes(r)
    if (r /= -1) error stop 3
    allocate(h%it)
    h%it%v = 8
    call consumes(r, h%it)
    if (r /= 8) error stop 4

    if (get_base(c) /= -1) error stop 5
    allocate(c)
    c%v = 4
    c%w = 10
    if (get_base(c) /= 14) error stop 6
    print *, "ok"
end program
