program intrinsics_61
    implicit none

    integer(1) :: x
    integer(2) :: y
    integer(4) :: z
    integer(8) :: w
    x = 97
    y = 47
    z = 56
    w = 67
    print *, char(x), char(y), char(z), char(w)

    if (char(x) /= 'a') error stop
    if (char(y) /= '/') error stop
    if (char(z) /= '8') error stop
    if (char(w) /= 'C') error stop
end program
