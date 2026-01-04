program integerStringInput_int64
    implicit none

    integer, parameter :: int64 = selected_int_kind(18)
    integer(int64) :: x

    open(unit=10, file="tests/invalidInput_integer.txt", status="unknown")

    do
        read(10, *, end=100) x
        print *, "Read int64 integer:", x
    end do

100 continue
    close(10)
    stop
end program integerStringInput_int64
