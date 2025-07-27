program separate_compilation_20
    use stdlib_error_separate_compilation_20
    implicit none

    integer :: tester = 1
    call error_stop(tester)

    print *, tester
    if (tester /= 3) error stop
end program