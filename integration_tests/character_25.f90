program character_25
    implicit none

    character(len=1), dimension(5) :: array
    character(len=:), allocatable :: str

    str = "12345"

    array(:) = str
    if (array(1) /= '1') error stop
    if (array(2) /= '1') error stop
    if (array(3) /= '1') error stop
    if (array(4) /= '1') error stop
    if (array(5) /= '1') error stop
    print *, array
end program character_25
