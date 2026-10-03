program char_array_02
    implicit none

    character(*), parameter :: words(*) = [character(7) ::]
    character(*), parameter :: names(*) = [character(5) :: "alpha", "beta"]

    if (size(words) /= 0) stop 1
    if (len(words) /= 7) stop 2
    if (size(names) /= 2) stop 3
    if (len(names) /= 5) stop 4
    if (names(1) /= "alpha") stop 5
    if (names(2) /= "beta ") stop 6
    print *, "test passed"
end program char_array_02
