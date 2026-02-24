program read_44

    type :: inner_t
        integer :: a
        integer :: b
    end type inner_t
    type :: outer_t
        integer :: id
        type(inner_t) :: inner
    end type outer_t
    type(outer_t) :: x
    character(len=12) :: s
    s = "001002003"

    read(s, "(I3,I3,I3)") x

    if (x%id /= 1) error stop 1
    if (x%inner%a /= 2) error stop 2
    if (x%inner%b /= 3) error stop 3

end program read_44