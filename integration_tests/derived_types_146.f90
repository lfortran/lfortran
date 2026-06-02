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
    print *, "ok"
end program
