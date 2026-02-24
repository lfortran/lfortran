program deferred_shape_01
    implicit none

    integer, dimension(*), parameter :: arr = [1, 2, 3]
    character(*), dimension(*), parameter :: keys = [character(5) :: &
        & "one", "two", "three" &
        & ]

    if (size(arr) /= 3) error stop
    if (arr(1) /= 1) error stop
    if (arr(2) /= 2) error stop
    if (arr(3) /= 3) error stop

    if (size(keys) /= 3) error stop
    if (trim(keys(1)) /= "one") error stop
    if (trim(keys(2)) /= "two") error stop
    if (trim(keys(3)) /= "three") error stop
end program
