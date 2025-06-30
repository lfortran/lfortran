program class_55
    type coor
        integer :: x
        integer :: y
    end type

    class(*), allocatable :: c

    type(coor), allocatable :: i
    i = coor(11, 22)

    allocate(c, source = i)

    select type (c)
        type is (integer)
            error stop
        type is (real)
            error stop
        type is (coor)
            if (c%x /= 11) error stop
            print *, c
        class default
            error stop
    end select

    deallocate(c)

    allocate(c, source = 4)
    select type (c)
        type is (integer)
            if (c /= 4) error stop
            print *, c
        type is (real)
            error stop
        type is (coor)
            error stop
        class default
            error stop
    end select

end program
