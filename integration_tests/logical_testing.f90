program logicalStringInput
    implicit none
    integer, parameter :: n = 6
    logical :: x
    character(len=1) :: actual(n), expected(n)
    integer :: ios, i
    character(len=20), dimension(n) :: inputs = [ &
        ".true.         ", ".false.        ", "true           ", "false          ", &
        "True           ", "False          " ]

    ! as suggested, creating array for the expected tests 
    expected = ['T', 'F', 'T', 'F', 'T', 'F']

    ! as suggested, using the scratch file over .txt file
    open(unit=10, status="scratch")

    ! writing in the scratch file
    do i = 1, n
        write(10, '(A)') trim(inputs(i))
    end do

    rewind(10)

    ! reading
    do i = 1, n
        read(10, *, iostat=ios) x
        if (ios /= 0) then
            print *, "Error reading logical at index", i
            stop 1
        end if
        if (x) then
            actual(i) = 'T'
        else
            actual(i) = 'F'
        end if
    end do

    close(10)

    ! array se check validation
    do i = 1, n
        if (actual(i) /= expected(i)) then
            print *, "Test failed at index", i
            print *, "Expected:", expected(i), " but got:", actual(i)
            stop 1
        end if
    end do

    print *, "Tests Working Correctly!"
end program logicalStringInput
