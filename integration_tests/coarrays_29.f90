program coarrays_29
    implicit none

    integer, allocatable :: x[:]

if (allocated(x)) error stop

    allocate(x[*])

if (.not. allocated(x)) error stop

    x = this_image()
    
    sync all
    
    if (this_image() == 1) then
        print *, x[2]
        if (x[2] /= 2) error stop
    end if

    deallocate(x)
end program coarrays_29