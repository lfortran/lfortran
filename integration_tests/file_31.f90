program file_31
    use iso_fortran_env, only: int16
    integer(int16), allocatable :: d(:,:)
    integer(int16), allocatable :: a(:)
    integer :: i, ios, s, j

    allocate(d(3,3))
    allocate(a(3))

    ! open a file and read data into the array
    open(unit=s, file='file_31_data.txt', status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file"
        stop
    end if
    read(s, *, iostat=ios) a
    if (ios /= 0) then
        print *, "Error reading data"
        stop
    end if
    do i = 1, 3
        do j = 1, 3
            read(s, *, iostat=ios) d(i, j)
            if (ios /= 0) then
                print *, "Error reading data"
                stop
            end if
        end do
    end do
    close(s)
    print *, "Loaded data:"
    do i = 1, 2
        print *, d(i, :)
    end do
    print *, sum(d)
    if ( sum(d) /= 1440 ) error stop
    print *, sum(a)
    if ( sum(a) /= 60 ) error stop
end program file_31
