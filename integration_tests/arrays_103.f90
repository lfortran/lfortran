! Test list-directed output of array section (issue #4705)
! The bug was missing spaces between array items in list-directed output
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

    ! Verify spacing: list-directed output should have separators between items
    ! The output should NOT be "4322147483647" (no spaces)
    if (index(trim(adjustl(output)), '4 32') == 0 .and. &
        index(trim(adjustl(output)), '4  32') == 0) then
        error stop "Missing space between output items"
    end if
end program arrays_103
