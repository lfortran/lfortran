program string_36
    implicit none

    character(len=5) :: hello

    hello = "hello"
    hello(4:3) = ""

    print *, hello

    if (hello /= "hello") error stop

end program
