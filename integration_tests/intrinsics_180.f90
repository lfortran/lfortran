program intrinsics_180
    implicit none

    character(len=20) :: x
    integer :: pos
    x = "fortran"

    pos = scan(x, "ao")
    print *, pos
    if (pos /= 2) error stop

    pos = scan(x, "a")
    print *, pos
    if (pos /= 6) error stop

    pos = scan(x, "ao", .true.)
    print *, pos
    if (pos /= 6) error stop

end program
