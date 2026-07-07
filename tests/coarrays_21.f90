program coarrays_21
    implicit none

    integer :: a(4)[*], b(4)[*] = 0
    integer :: dest
    integer :: i = 1
    integer :: expected
    expected = mod(this_image() + num_images() - 2, num_images()) + 1
    a = this_image()

    dest = mod(this_image(), num_images()) + 1

    b(2)[dest] = a(2)

    sync all
    do i = 1, 4
        if (i == 2) then
            if (b(i) /= expected) error stop
        else
            if (b(i) /= 0) error stop
        end if
    end do
end program coarrays_21