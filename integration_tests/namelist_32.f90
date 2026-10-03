program namelist_32
    implicit none
    integer :: x, y, a, b, c, d

    namelist /left/ x, a /right/ y, b
    namelist /first/ c, /second/ d
    namelist /left/ c

    x = 1
    y = 2
    a = 3
    b = 4
    c = 5
    d = 6

    write(*, nml=left)
    write(*, nml=right)
    write(*, nml=first)
    write(*, nml=second)

    if (x /= 1 .or. y /= 2 .or. a /= 3) error stop
    if (b /= 4 .or. c /= 5 .or. d /= 6) error stop
    print *, "multiple namelist groups passed"
end program namelist_32
