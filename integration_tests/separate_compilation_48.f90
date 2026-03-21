program separate_compilation_48
    use mod_separate_compilation_48
    implicit none
    type(mytype) :: t
    real, allocatable :: p(:)
    integer :: i

    allocate(t%weights(2, 3))
    t%weights = 1.0

    p = get_params(t)

    if (size(p) /= 6) error stop "FAIL: wrong size"
    do i = 1, size(p)
        if (abs(p(i) - 1.0) > 1e-6) error stop "FAIL: wrong value"
    end do
    print *, "separate_compilation_48: all tests passed."

end program separate_compilation_48
