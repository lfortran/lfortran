program data_18
    implicit none
    real :: array(5)
    integer, parameter :: nelems = 5
    integer :: i
    DATA (array(i), i=1,5) /nelems*3.1415/

    do i = 1, 5
        if (abs(array(i) - 3.1415) > 1.0e-5) error stop
    end do
end program data_18
