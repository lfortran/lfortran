program coarrays_04
    ! run with 2 images
    implicit none

    integer :: me, n

    me = this_image()
    n = num_images()

    if (n /= 2) error stop 1 ! can test with different images

    if (me < 1 .or. me > n) error stop 2

end program coarrays_04