program file_32
    implicit none
    complex, allocatable :: array(:,:,:,:)
    integer :: stat, io

    ! Allocate a 4D complex array of shape (2, 2, 1, 1) = 4 elements
    allocate(array(2,2,1,1))

    ! Open text file
    open(newunit=io, file="file_32_data.txt", status="old", action="read", iostat=stat)
    if (stat /= 0) then
        print *, "Error opening file"
        stop 1
    end if

    ! Directly read entire array in one READ statement
    read(io, *, iostat=stat) array
    if (stat /= 0) then
        print *, "Error reading array"
        stop 1
    end if

    close(io)

    ! Print to verify
    print *, "array(1,1,1,1) = ", abs(array(1,1,1,1))
    if ( abs(abs(array(1,1,1,1)) - 0.0) > 1e-6 ) error stop
    print *, "array(2,1,1,1) = ", abs(array(2,1,1,1))
    if ( abs(abs(array(2,1,1,1)) - 2.0) > 1e-6 ) error stop
    print *, "array(1,2,1,1) = ", abs(array(1,2,1,1))
    if ( abs(abs(array(1,2,1,1)) - 1.0) > 1e-6 ) error stop
    print *, "array(2,2,1,1) = ", abs(array(2,2,1,1))
    if ( abs(abs(array(2,2,1,1)) - 3.0) > 1e-6 ) error stop
end program
