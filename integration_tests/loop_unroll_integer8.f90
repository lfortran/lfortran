program loop_unroll_integer8
    ! A loop over an integer(8) index that the loop_unroll pass unrolls:
    ! the rewritten end bound must keep the kind of the index.
    implicit none
    integer(8) :: i
    integer :: array(33)

    do i = 1_8, 33_8
        array(i) = int(i)
    end do

    print *, sum(array)
    if (sum(array) /= 561) error stop
    if (array(1) /= 1 .or. array(33) /= 33) error stop
end program
