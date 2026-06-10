program write_41
    implicit none

    character(2) :: c(5)

    c = "??"
    write(c(5:1:-2), "(A, I0)") "x", 2, "z"
    if (c(1) /= "??") error stop
    if (c(2) /= "??") error stop
    if (c(3) /= "z ") error stop
    if (c(4) /= "??") error stop
    if (c(5) /= "x2") error stop

    c = "??"
    write(c(5:1:-2), "(A,A)") "x", "V", "z"
    if (c(1) /= "??") error stop
    if (c(2) /= "??") error stop
    if (c(3) /= "z ") error stop
    if (c(4) /= "??") error stop
    if (c(5) /= "xV") error stop
end program