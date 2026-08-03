program coarrays_35
    implicit none

    integer, allocatable :: x[:,:,:]

    if (allocated(x)) error stop

    allocate(x[11:12,21:22,*])

    if (.not. allocated(x)) error stop

    if (any(lcobound(x) /= [ 11, 21, 1 ])) error stop "Wrong lcobounds"
    if (ucobound(x,1) /= 12) error stop "Wrong ucobound 1"
    if (ucobound(x,2) /= 22) error stop "Wrong ucobound 1"

    x = this_image()

    sync all

    if (this_image() == 1) then
        print *, x[12,21,1]
        if (x[12,21,1] /= 2) error stop
    end if

    deallocate(x)
end program coarrays_35