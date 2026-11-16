program coarrays_02
    implicit none
    integer :: a(5)[*] = 0
    integer :: i

    do i = 1, 5
        a(i) = this_image() * 10 + i
    end do

    sync all

    if (any(a(2:4)[1] /= [12, 13, 14])) error stop
end program
