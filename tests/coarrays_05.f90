program coarrays_05
    implicit none

    integer :: me, n
    integer(8) :: x[*]

    me = this_image()
    n = num_images()

    if (me == 1) x[2] = 30
    if (me == 2) x[1] = 42

    print *, "me :", me, "x[me] :", x[me]
end program coarrays_05