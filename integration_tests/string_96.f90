program string_96
    character(len=:), allocatable :: long_short(:)
    integer :: isize
    isize = 3
    allocate(character(len=10) :: long_short(3))
    long_short(1) = "first"
    long_short(2) = "second"
    long_short(3) = "third"
    long_short=long_short(:isize-1)
    print *, long_short
end program string_96