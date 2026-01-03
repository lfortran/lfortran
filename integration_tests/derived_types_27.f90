program derived_types_27
    type :: int_container
        integer, dimension(2) :: my_int
    end type
    integer :: my_int, i

    type(int_container) :: t
    my_int = 12
    t%my_int(:) = 39

    print *, t%my_int(:), my_int
    do i = 1, 2
        if (t%my_int(i) /= 39) error stop
    end do
    if (my_int /= 12) error stop
end program
