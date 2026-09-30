! ASSOCIATE and SELECT TYPE with module-qualified selectors.
module namespace_modules_20_state
    implicit none
    type :: item_t
        integer :: count = 0
    end type
    integer :: total = 10
    real :: grid(2, 2) = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
    class(*), allocatable :: anything
    type(item_t) :: item
end module

program namespace_modules_20
    use, namespace :: st => namespace_modules_20_state
    implicit none

    associate (t => st%total, g => st%grid, c => st%item%count)
        t = t + 5
        g(2, 2) = 40.0
        c = 3
    end associate
    if (st%total /= 15) error stop
    if (abs(st%grid(2, 2) - 40.0) > 1e-6) error stop
    if (st%item%count /= 3) error stop

    st%anything = 7
    select type (a => st%anything)
    type is (integer)
        if (a /= 7) error stop
    class default
        error stop
    end select

    st%anything = st%item_t(9)
    select type (a => st%anything)
    type is (integer)
        error stop
    type is (st%item_t)
        if (a%count /= 9) error stop
    class default
        error stop
    end select
    print *, st%total, st%grid(2, 2), st%item%count
end program
