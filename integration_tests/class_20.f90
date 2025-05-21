module class_20_m
    implicit none

    type base 
        integer :: m = 44
    end type

    type, extends(base) :: derived
        integer :: n = 45
    end type
end module

program class_20
    use class_20_m
    implicit none

    class(base), allocatable :: b
    allocate(derived :: b)

    if (b%m /= 44) error stop
end program
