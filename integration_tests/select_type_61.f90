module select_type_61_mod
    implicit none
    type :: base
        integer :: id = 1
    end type
    type, extends(base) :: item
        integer :: n = 7
    end type
    type, extends(item) :: special
        integer :: m = 11
    end type
contains
    subroutine check_element(arr)
        class(base), intent(inout) :: arr(:)
        select type (x => arr(2))
        class is (item)
            if (x%n /= 7) error stop 10
            if (x%id /= 1) error stop 11
            x%n = 21
        end select
    end subroutine

    subroutine check_scalar(s, hits)
        class(base), intent(in) :: s
        integer, intent(inout) :: hits
        select type (y => s)
        class is (item)
            if (y%n /= 21) error stop 12
            hits = hits + 1
        end select
    end subroutine
end module

program select_type_61
    use select_type_61_mod
    implicit none
    class(base), allocatable :: a(:)
    class(base), allocatable, target :: b(:)
    class(base), pointer :: p(:)
    integer :: hits

    allocate(item :: a(1))
    hits = 0
    select type (x => a(1))
    class is (item)
        print *, x%n
        if (x%n /= 7) error stop 1
        x%n = 42
        hits = hits + 1
    end select
    if (hits /= 1) error stop 2
    select type (x => a(1))
    type is (item)
        if (x%n /= 42) error stop 3
    end select

    allocate(special :: b(3))
    select type (y => b(3))
    class is (item)
        if (y%n /= 7) error stop 4
        if (y%id /= 1) error stop 5
        hits = hits + 1
    end select
    if (hits /= 2) error stop 6

    call check_element(b)
    call check_scalar(b(2), hits)
    if (hits /= 3) error stop 7
    select type (z => b(2))
    type is (special)
        if (z%n /= 21) error stop 8
        if (z%m /= 11) error stop 9
    end select

    p => b
    select type (w => p(1))
    class is (special)
        if (w%m /= 11) error stop 13
        w%m = 5
        hits = hits + 1
    end select
    if (hits /= 4) error stop 14
    select type (v => b(1))
    type is (special)
        if (v%m /= 5) error stop 15
    end select
    print *, "ok"
end program
