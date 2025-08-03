program separate_compilation_21
    use math_separate_compilation_21
    implicit none

    integer, parameter :: n = 2
    integer, parameter :: base = 1
    real :: result(n)

    result = func(n, base)

    print *, result
    if (.not. all(result == [1.0, 2.0])) error stop
end program