program realStringInput
    implicit none
    real :: x
    open(unit=10, file="../invalidInput_float.txt", status="unknown")

    do
        read(10, *, end=100) x
        print *, "Read real number:", x
    end do

100 continue
    close(10)
    stop

end program realStringInput
