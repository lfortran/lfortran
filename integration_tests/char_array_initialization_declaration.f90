program main
    character(len=2) :: c(2) = "ap"
    character(len=3), dimension(2) :: x = "app"
    print *, c
    if (c(1) /= "ap") error stop
    if (c(2) /= "ap") error stop

    print *, x
    if (x(1) /= "app") error stop
    if (x(2) /= "app") error stop
    if (x(3) /= "app") error stop
end program
