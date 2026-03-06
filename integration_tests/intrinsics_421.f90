program intrinsics_421
    implicit none
    integer :: img

    img = this_image()

    if (img < 1) error stop

    print *, "PASS"
end program intrinsics_421