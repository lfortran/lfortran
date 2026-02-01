! Test list-directed output of array section (issue #4705)
program arrays_103
    implicit none
    integer, parameter :: k = selected_int_kind(9)
    integer :: iarray(3, 4)
    character(100) :: output

    iarray = 0
    iarray(1, 1:2) = [bit_size(1_k), huge(1_k)]

    write(output, *) kind(1_k), iarray(1, 1:2)
    if (iarray(1, 1) /= 32) error stop
    if (iarray(1, 2) /= 2147483647) error stop
end program arrays_103
