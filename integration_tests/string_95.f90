program string_95
    ! Test string length computation for implied-do loops in array constructors
    ! Uses same-length strings to be compatible with gfortran behavior
    implicit none
    character(len=10) :: lines(5)
    character(len=:), allocatable :: result(:)
    integer :: i

    ! All strings have the same trimmed length to avoid gfortran length quirks
    lines(1) = "apple"
    lines(2) = "peach"
    lines(3) = "grape"
    lines(4) = "mango"
    lines(5) = "lemon"

    ! Array constructor with implied-do over character array
    result = [(trim(lines(i)), i=1,5)]

    if (size(result) /= 5) error stop "Size mismatch"
    if (trim(result(1)) /= "apple") error stop "Element 1 mismatch"
    if (trim(result(2)) /= "peach") error stop "Element 2 mismatch"
    if (trim(result(3)) /= "grape") error stop "Element 3 mismatch"
    if (trim(result(4)) /= "mango") error stop "Element 4 mismatch"
    if (trim(result(5)) /= "lemon") error stop "Element 5 mismatch"

    print *, "PASS"
end program string_95
