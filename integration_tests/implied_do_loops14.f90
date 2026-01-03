program implied_do_loops14
    implicit none

    character(:), allocatable :: tmp_line
    character(len=5) :: values(3)
    character(len=5) :: padded
    character(:), allocatable :: trimmed(:)

    integer :: i

    tmp_line = "Hello"
    values = [(tmp_line, i = 1, 3)]

    do i = 1, 3
        if (values(i) /= "Hello") error stop 1
    end do

    padded = "Hell "

    allocate(character(len=len(trim(padded))) :: trimmed(3))
    trimmed = [(trim(padded), i = 1, 3)]

    do i = 1, 3
        if (trimmed(i) /= "Hell") error stop 2
    end do

    print *, (trim(padded), i = 1, 3)
end program implied_do_loops14
