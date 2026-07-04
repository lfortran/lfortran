! run with 2 images
program coarrays_20
    integer :: a(3)[*]
    integer :: i

    do i = 1, 3
        a(i) = this_image() * 10 + i
    end do

    sync all

    if (this_image() == 1) then
        if (a(1)[2] /= 21) error stop
        if (a(2)[2] /= 22) error stop
        if (a(3)[2] /= 23) error stop
    end if
end program coarrays_20