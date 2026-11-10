program transfer_36
    ! transfer with a large size argument (1 MiB result) must not overflow
    ! the stack
    implicit none

    integer, parameter :: rand_size = 2**18
    integer, parameter :: test_size = rand_size*4
    integer(4) :: rand_object(rand_size)
    integer(1) :: test_8_bits(test_size)
    integer(1), parameter :: int8_0 = 0
    integer(4) :: back(rand_size)
    integer :: i

    do i = 1, rand_size
        rand_object(i) = i
    end do
    test_8_bits(:) = transfer( rand_object, int8_0, test_size )
    back = transfer( test_8_bits, 0_4, rand_size )
    if (any(back /= rand_object)) error stop
    if (count(test_8_bits /= 0_1) == 0) error stop
    print *, back(1), back(rand_size)
end program transfer_36
