module optional_18_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type, extends(item_t) :: sub_t
        integer :: w = 0
    end type
contains
    function make(v) result(item)
        integer, intent(in) :: v
        class(item_t), allocatable :: item
        allocate(item_t :: item)
        item%v = v
    end function

    function make_sub(v) result(item)
        integer, intent(in) :: v
        class(item_t), allocatable :: item
        allocate(sub_t :: item)
        item%v = v
        select type (item)
        type is (sub_t)
            item%w = 2*v
        end select
    end function

    function make_any(v) result(item)
        integer, intent(in) :: v
        class(*), allocatable :: item
        allocate(item, source=item_t(v))
    end function

    function make_any_int(v) result(item)
        integer, intent(in) :: v
        class(*), allocatable :: item
        allocate(item, source=v)
    end function

    subroutine consume_any(r, item)
        integer, intent(out) :: r
        class(*), optional :: item
        r = -1
        if (present(item)) then
            select type (item)
            type is (item_t)
                r = item%v
            type is (integer)
                r = 100 + item
            end select
        end if
    end subroutine

    subroutine consume(r, item)
        integer, intent(out) :: r
        class(item_t), optional :: item
        r = -1
        if (present(item)) then
            r = item%v
            select type (item)
            type is (sub_t)
                r = r + item%w
            end select
        end if
    end subroutine

    subroutine test()
        integer :: r
        call consume(r, make(5))
        if (r /= 5) error stop
        call consume_any(r, make_any(5))
        if (r /= 5) error stop
    end subroutine
end module

program optional_18
    use optional_18_mod, only: make, make_sub, consume, test, &
        make_any, make_any_int, consume_any
    implicit none
    integer :: r
    call consume(r, make(7))
    if (r /= 7) error stop
    call consume(r, make_sub(7))
    if (r /= 21) error stop
    call consume(r)
    if (r /= -1) error stop
    call consume_any(r, make_any(7))
    if (r /= 7) error stop
    call consume_any(r, make_any_int(7))
    if (r /= 107) error stop
    call consume_any(r)
    if (r /= -1) error stop
    call test()
    print *, "ok"
end program
