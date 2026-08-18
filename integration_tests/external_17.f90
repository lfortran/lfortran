program external_mangle_separate_01
    implicit none
    integer :: x
    x = 41
    call dummy_external(x)
    if (x /= 42) error stop
end program
