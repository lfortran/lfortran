program test_cosum
    integer :: x

    x = this_image()

    call co_sum(x)

    if (x /= 3) error stop ! run with 2 images
end program