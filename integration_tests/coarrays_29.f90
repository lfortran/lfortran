program coarrays_29
    implicit none

    integer, allocatable :: x[:]

    allocate(x[*])

    x = this_image()
    
    if (this_image() == 1) then
        print *, x[2]
    end if

    deallocate(x)
end program coarrays_29