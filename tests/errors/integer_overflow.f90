program integer_overflow
    implicit none
    integer :: x, unit_no
    character(len=100) :: filename

    open(newunit=unit_no, status='scratch', iostat=x)
    if (x /= 0) then
        print *, "Failed to open scratch file."
        stop
    end if

    ! writing in file
    write(unit_no, *) 1729
    write(unit_no, *) 2147483648_8  ! using _8 (for int64) because, when writing in int32 mode, while writing, it is parsed wrong

    rewind(unit_no)

   do
        read(unit_no, *, end=100) x
        print *, "Read integer:", x
    end do

100 continue
    close(unit_no)
end program integer_overflow
