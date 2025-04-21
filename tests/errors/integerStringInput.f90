program integerStringInput
    implicit none
    integer :: x
    open(unit=10, file="../invalidInput_integer.txt", status="unknown")

    do
        read(10, *, end=100) x
        print *, "Read integer:", x
    end do

100 continue
    close(10)
    stop

end program integerStringInput