program realStringInput
    implicit none
    real :: x
    integer :: ios, lineno
    character(len=100) :: dummy

    lineno = 0
    open(unit=10, file="invalidInput_float.txt", status="old")

    do
        lineno = lineno + 1
        read(10, *, iostat=ios) x
        if (ios /= 0) then
            if (ios == -1) exit
            print *, "Error at line", lineno, ": Invalid real input"
            read(10, '(A)', iostat=ios) dummy
            cycle
        end if
        print *, "Line", lineno, ": Read real number:", x
    end do

    close(10)
end program realStringInput
