program derived_types_146
    implicit none

    type :: box
        integer, allocatable :: x
    end type

    type(box) :: a, b

    a = box(null())
    b = box(x=null())

    if (allocated(a%x)) error stop
    if (allocated(b%x)) error stop
    if (func(null()) /= 0) error stop
    if (func(v=null()) /= 0) error stop
    print *, "ok"

contains
    integer function func(v)
        integer, intent(in), pointer :: v
        func = 0
    end function
end program
