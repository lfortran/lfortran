program implied_do_loops45
    implicit none

    integer :: i
    character(len=1), parameter :: chars(*) = [(achar(iachar("A")), i = 1, 1)]

    if (size(chars) /= 1) error stop 1
    if (chars(1) /= "A") error stop 2
    print *, chars
end program implied_do_loops45
