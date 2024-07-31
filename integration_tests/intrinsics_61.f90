program intrinsics_61
    implicit none

    integer(1) :: x
    integer(2) :: y
    integer(4) :: z
    integer(8) :: w
    character(1), parameter :: c1 = char(32)
    character(1), parameter :: c2 = char(57, 4)
    character(1), parameter :: ar1(4) = char([32, 57, 56, 67])
    ! character(1), parameter :: ar2(4) = char([32, 57, 56, 67], 4) !Does not work #4566
    integer :: arr1(4) = [32, 55, 87, 90]

    print *, c1
    if (c1 /= ' ') error stop
    print *, c2
    if (c2 /= '9') error stop
    print *, ar1
    if (any(ar1 /= [' ', '9', '8', 'C'])) error stop
    ! print *, ar2
    ! if (any(ar2 /= [' ', '9', '8', 'C'])) error stop
    print *, char(arr1)
    if (any(char(arr1) /= [' ', '7', 'W', 'Z'])) error stop

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
