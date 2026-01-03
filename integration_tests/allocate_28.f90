program allocate_28
    implicit none
    type :: my_type
        integer :: x
    end type
    class(my_type), allocatable :: obj
    class(my_type), allocatable :: original
    allocate(my_type :: original)
    original%x = 123
    allocate(obj, mold=original)
    obj%x = 456
    print *, "obj%x =", obj%x
    if (obj%x /= 456) error stop
end program allocate_28