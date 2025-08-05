program separate_compilation_22
    use math_separate_compilation_22
    implicit none

    integer :: n = 2
    call logspace(n)

    print *, n
    if (n /= 3) error stop
end program