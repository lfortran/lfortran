program integerStringInput
    implicit none
    integer :: x, ios, lineno
    character(len=100) :: dummy

    lineno = 0
    open(unit=10, file="invalidInput_integer.txt", status="old")

    do
        lineno = lineno + 1
        read(10, *, iostat=ios) x
        if (ios /= 0) then
            if (ios == -1) exit
            print *, "Error at line", lineno, ": Invalid integer input"
            read(10, '(A)', iostat=ios) dummy
            cycle
        end if
        print *, "Line", lineno, ": Read integer:", x
    end do

    close(10)
end program integerStringInput
