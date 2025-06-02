program file_32
    implicit none
    complex, allocatable :: array(:,:,:,:)
    integer :: stat, io
    character(len=200) :: line
    character(len=20) :: part
    real :: c1(2), c2(2), c3(2), c4(2)

    ! Allocate a 4D complex array of shape (2, 2, 1, 1) = 4 elements
    allocate(array(2,2,1,1))

    ! Sample values
    c1 = [0.0, 0.0]
    c2 = [2.0, 0.0]
    c3 = [1.0, 0.0]
    c4 = [3.0, 0.0]

    ! Open file
    open(newunit=io, file="file_32_data.txt", status="replace", action="write", iostat=stat)
    if (stat /= 0) then
        print *, "Error opening file for writing"
        stop 1
    end if

    ! Initialize line
    line = ''

    ! Append each point with a space after it
    write(part, '(f3.1,",",f3.1)') c1(1), c1(2)
    line = trim(line) // '(' // trim(part) // ') '
    print *, "line1 = ", trim(line)

    write(part, '(f3.1,",",f3.1)') c2(1), c2(2)
    line = trim(line) // ' ' // '(' // trim(part) // ') '
    print *, "line2 = ", trim(line)

    write(part, '(f3.1,",",f3.1)') c3(1), c3(2)
    line = trim(line) // ' ' // '(' // trim(part) // ') '

    write(part, '(f3.1,",",f3.1)') c4(1), c4(2)
    line = trim(line) // ' ' // '(' // trim(part) // ')'

    ! Write final line
    write(io, '(a)') trim(line)
    close(io)

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
