program nested_read

    type :: inner_t
        integer :: a
        character(20) :: b
    end type inner_t

    type :: outer_t
        integer :: id
        type(inner_t) :: inner
    end type outer_t

    type(outer_t) :: x

    print *, "Enter: id, a, b"
    read(*,*) x

    print *, "id =", x%id
    print *, "a  =", x%inner%a
    print *, "b  =", x%inner%b

end program nested_read
