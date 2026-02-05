! Test list-directed output of array section (issue #4705)
! The bug was missing spaces between array items in list-directed output
! Original bug: output was "4\n322147483647" (values concatenated, no spaces)
program arrays_104
    implicit none
    integer, parameter :: k = selected_int_kind(9)
    integer :: iarray(3, 4)
    character(100) :: output

    iarray = 0
    iarray(1, 1:2) = [bit_size(1_k), huge(1_k)]

    write(output, *) kind(1_k), iarray(1, 1:2)
    if (iarray(1, 1) /= 32) error stop
    if (iarray(1, 2) /= 2147483647) error stop

    ! Verify spacing: list-directed output must have separators between items
    ! The bug was values concatenated without any spaces: "432" or "322147483647"
    ! Check that these concatenated patterns do NOT appear
    if (index(output, '432') /= 0) then
        error stop "Values 4 and 32 are concatenated without space"
    end if
    if (index(output, '322147483647') /= 0) then
        error stop "Values 32 and 2147483647 are concatenated without space"
    end if
end program arrays_104
