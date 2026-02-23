program read_44

    type :: inner_t
        integer :: a
        character(20) :: b
    end type inner_t

    type :: outer_t
        integer :: id
        type(inner_t) :: inner
    end type outer_t

    type(outer_t) :: x
    character(len=20) :: s1, s2, s3

    ! Simulated CI input (3 separate internal reads)
    s1 = "1"
    s2 = "10"
    s3 = "hello"

    read(s1, *) x%id
    read(s2, *) x%inner%a
    read(s3, *) x%inner%b

    if (x%id /= 1) error stop 1
    if (x%inner%a /= 10) error stop 2
    if (trim(x%inner%b) /= "hello") error stop 3

end program read_44