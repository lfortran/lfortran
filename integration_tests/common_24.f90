program common_24
    implicit none

    integer :: info
    integer :: iunit

    common /infoc/ info, iunit

    info = 0
    iunit = 0

    call set1()
    call set2()

    if (info /= 2) error stop
end program common_24
