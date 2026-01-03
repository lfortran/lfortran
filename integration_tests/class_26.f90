program class_26

    implicit none

    type :: other_type
        class(*), allocatable :: value
        integer :: val = 1
    end type other_type

    type(other_type) :: other

    print * , other % val
    if (other % val /= 1) error stop
end program
