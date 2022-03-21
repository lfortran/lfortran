program loop_unroll_large

    implicit none
    integer :: i, first, last
    integer, allocatable :: array(:)

    allocate(array(35))

    do i = 1, 35
        array(i) = i
    end do

    do i = 1, 35
        print *, array(i)
    end do

end program

