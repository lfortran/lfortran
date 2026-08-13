program initializer_noncompile_time_12120
    implicit none
    integer :: expected = mod(this_image() + num_images() - 2, num_images()) + 1
    print *, expected
end program
