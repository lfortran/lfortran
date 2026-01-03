module class_49_m_1
    implicit none

    type base
        integer :: m = 1
    end type
end module

module class_49_m_2
    use class_49_m_1, only: base

    type, extends(base) :: derived
        integer :: n = 2
    end type
end module

program class_49
    use class_49_m_1, only: base
    use class_49_m_2, only: derived

    class(base), allocatable :: b
    allocate(derived :: b)
    b = derived(10, 20)

    select type(b)
        type is (derived)
            if (b%m /= 10) error stop
            if (b%n /= 20) error stop
        class default
            error stop
    end select
end program
