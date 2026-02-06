! Test array section assignment with different integer kind (issue #4501)
program arrays_103
    implicit none
    integer, parameter :: k = selected_int_kind(0)
    integer :: iarray(3, 4)

    iarray = 0
    iarray(1, 1:2) = [bit_size(1_k), huge(1_k)]

    if (iarray(1, 1) /= 8) error stop
    if (iarray(1, 2) /= 127) error stop
end program arrays_103
