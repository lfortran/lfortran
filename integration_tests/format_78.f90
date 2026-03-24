program format_78
    implicit none
    character(len=10) :: dec
    character(len=20) :: str
    real :: x, y
    integer :: ios

    x = 3.14

    ! Open file with decimal comma mode
    open(unit=10, file="test.txt", status="replace", &
         decimal="comma", form="formatted", action="readwrite")

    ! Write value
    write(10, '(F6.2)') x

    ! Check decimal mode
    inquire(unit=10, decimal=dec)
    if (trim(dec) /= "COMMA") then
        print *, "FAIL: decimal mode is ", trim(dec)
        error stop
    end if

    ! Rewind to read back
    rewind(10)

    ! Read as string
    read(10, '(A)', iostat=ios) str
    if (ios /= 0) then
        print *, "FAIL: could not read string"
        error stop
    end if

    ! Check if comma is present
    if (index(str, ",") == 0) then
        print *, "FAIL: comma not found in written output:", trim(str)
        error stop
    end if

    close(10)

end program format_78
