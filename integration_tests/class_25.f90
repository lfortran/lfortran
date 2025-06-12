module class_25_m
    implicit none

    type base 
        integer :: m = 1
    end type

    type, extends(base) :: derived
        integer :: n = 2
    end type
end module

program class_25
    use class_25_m
    implicit none

    class(base), allocatable :: b

    b = derived(10, 20)
    select type(b)
        type is (derived)
            if (b%m /= 10) error stop
            if (b%n /= 20) error stop
        class default
            error stop
    end select

    b = base(3)
    select type(b)
        type is (base)
            if (b%m /= 3) error stop
        class default
            error stop
    end select
end program
