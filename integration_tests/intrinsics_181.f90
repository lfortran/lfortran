program intrinsics_181
    implicit none

    character(len=2) :: x1
    character(len=4) :: x2
    x1 = "wh"
    x2 = "what"

    print *, repeat(x1, 3)
    if (repeat(x1, 3) /= "whwhwh") error stop

    print *, "|"//repeat(x2, 3)//"|"
    if (repeat(x2, 3) /= "whatwhatwhat") error stop

end program
