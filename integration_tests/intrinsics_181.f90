program intrinsics_181
    implicit none
    character(3), parameter :: s1 = repeat("a", 3)
    character(10), parameter :: s2 = repeat("#$", 5_8)
    character(len=2) :: x1
    character(len=4) :: x2

    x1 = "wh"
    x2 = "what"

    print *, s1
    if (s1 /= "aaa") error stop
    print *, s2
    if (s2 /= "#$#$#$#$#$") error stop

    print *, repeat(x1, 3)
    if (repeat(x1, 3) /= "whwhwh") error stop
    print *, "|"//repeat(x2, 3)//"|"
    if (repeat(x2, 3) /= "whatwhatwhat") error stop

end program
