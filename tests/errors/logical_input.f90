program logicalInput
    implicit none
    logical :: x
    integer :: ios

    open(unit=10, file="logicalTest.txt", status="old")

    do
        read(10, *, iostat=ios) x
        if (ios /= 0) exit
        print *, "Read logical:", x
    end do

    close(10)
end program logicalInput