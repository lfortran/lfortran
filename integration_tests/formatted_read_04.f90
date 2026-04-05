program read_68
    implicit none

    integer :: ios
    character(len=100) :: iomsg
    character(len=100) :: s
    complex :: d(3)

    s = "1.0 2.0 3.0 4.0 5.0 6.0"

    read (s, '(6F4.1)', iostat=ios, iomsg=iomsg) d(:)

    if (ios /= 0) then
        print *, trim(iomsg)
        error stop "read_68 iostat"
    end if

    if (d(1) /= (1.0, 2.0)) error stop "read_68 mismatch 1"
    if (d(2) /= (3.0, 4.0)) error stop "read_68 mismatch 2"
    if (d(3) /= (5.0, 6.0)) error stop "read_68 mismatch 3"

    print *, "read_68 passed"
end program read_68
