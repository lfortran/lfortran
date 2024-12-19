program string_43

    implicit none
    character(:), allocatable :: a

    a = "Hello"

    print *, a
    if(a /= "Hello") error stop

end program string_43