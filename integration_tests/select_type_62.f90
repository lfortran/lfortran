program select_type_62
    implicit none
    type :: base
        integer :: id = 3
    end type
    type, extends(base) :: item
        integer :: n = 7
    end type
    type, extends(base) :: other
        real :: r = 1.5
    end type
    class(base), allocatable :: a(:)
    integer :: hits

    allocate(other :: a(2))
    hits = 0
    select type (x => a(2))
    class is (item)
        error stop 1
    class default
        if (x%id /= 3) error stop 2
        x%id = 9
        hits = hits + 1
    end select
    if (hits /= 1) error stop 3

    select type (x => a(2))
    type is (item)
        error stop 4
    class default
        if (x%id /= 9) error stop 5
        hits = hits + 1
    end select
    if (hits /= 2) error stop 6

    select type (x => a(1))
    type is (other)
        if (x%id /= 3) error stop 7
        if (abs(x%r - 1.5) > 1e-6) error stop 8
        hits = hits + 1
    class default
        error stop 9
    end select
    if (hits /= 3) error stop 10
    print *, "ok"
end program
