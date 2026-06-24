program sync_memory_example
    implicit none

    integer :: data[*]
    integer :: flag[*]

    data = 0
    flag = 0

    if (this_image() == 1) then
        data[2] = 42
        sync memory
        flag[2] = 1

    else if (this_image() == 2) then
        do while (flag /= 1)
        end do

        sync memory

        print *, data
    end if
end program