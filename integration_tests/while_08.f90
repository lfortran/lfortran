module while_08_mod
    implicit none

    type :: iterator
        integer :: limit = 0
    contains
        procedure :: next
    end type

    integer :: cnt = 0

contains

    function next(this, values) result(more)
        class(iterator), intent(inout) :: this
        integer, intent(out) :: values(:)
        logical(1) :: more

        values = this%limit
        more = this%limit > 0
        this%limit = this%limit - 1
    end function

    logical(1) function f(v)
        integer, intent(out) :: v(:)
        v = 7
        cnt = cnt + 1
        f = cnt < 4
    end function

end module

program while_08
    use while_08_mod
    implicit none
    integer, allocatable :: values(:)
    integer :: v(5), n
    type(iterator) :: it

    allocate(values(4))
    values = -1
    it%limit = 3
    n = 0
    do while (it%next(values(2:4)))
        values(1) = 1
        n = n + 1
    end do
    print *, n, values
    if (n /= 3) error stop
    if (values(1) /= 1) error stop
    if (any(values(2:4) /= 0)) error stop

    v = 0
    n = 0
    do while (f(v(2:4)))
        n = n + 1
    end do
    print *, n, cnt, v
    if (n /= 3) error stop
    if (cnt /= 4) error stop
    if (any(v /= [0, 7, 7, 7, 0])) error stop
end program
