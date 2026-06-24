program coarrays_11
    implicit none

    integer :: x[*]
    integer :: flag[*]

    x = 0
    flag = 0

    if (this_image() == 1) then
        x[2] = 123
        sync memory
        flag[2] = 1

    else if (this_image() == 2) then
        do while (flag == 0)
        end do

        sync memory

        if (x /= 123) then
            print *, "FAIL", x
        else
            print *, "PASS"
        end if
    end if
end program coarrays_11