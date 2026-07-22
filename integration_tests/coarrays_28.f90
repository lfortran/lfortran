program coarrays_28
    implicit none

    integer :: a(5)[*] = 0
    integer :: i

    do i = 1, 5
        a(i) = this_image() * 10 + i
    end do

    if (this_image() == 1) then
        print *, a(:)[2]
    end if
end program coarrays_28
