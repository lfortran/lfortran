program fileops
    implicit none
    integer :: j, i
    character(len=1) :: c
    integer(8) :: d
    real :: z
    j = 11
    open(j, file="file_01_data.txt")
    read(j, *) i
    read(j, *) c
    read(j, *) z
    read(j, *) d
    rewind(j)
    close(j)

    print *, i, c, z
    if (i /= 10130) error stop
    if (c /= "c") error stop
    if (abs(z - 4.20) > 1e-6) error stop
    if (d /= 4024052) error stop

end program fileops
