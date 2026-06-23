program sync_memory_mre
    implicit none

    integer :: data[*]
    integer :: ready[*]

    data = 0
    ready = 0

    if (this_image() == 1) then
        data[2] = 42
        sync memory
        ready[2] = 1
    else if (this_image() == 2) then
        do while (ready /= 1)
        end do

        sync memory
        print *, data
    end if

end program