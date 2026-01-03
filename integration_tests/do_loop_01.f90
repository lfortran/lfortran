program main
    implicit none
    integer :: j
    j = 0
    do
        j = j + 1
        if (j > 10) exit
        print *, j
    end do
    print *, "out of loop"
end program
