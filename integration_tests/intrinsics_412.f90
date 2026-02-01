! Test repeat() on allocatable string with large growth (issue #4660)
program intrinsics_412
    implicit none
    character(:), allocatable :: string
    integer :: n

    string = '0123456789'
    do n = 1, 5
        string = repeat(string, 10)
        if (len(string) /= 10**(n + 1)) error stop
    end do
end program intrinsics_412
