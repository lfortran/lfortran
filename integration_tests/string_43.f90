program string_43

    implicit none
    character(:), allocatable :: a
    character(:), allocatable :: b

    a = "Hello"
    a  =  b

    print *, a
    if(a /= "") error stop

end program string_43