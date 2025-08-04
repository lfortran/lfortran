program separate_compilation_21
    use math_separate_compilation_21
    implicit none

    integer, parameter :: n = 2
    integer, parameter :: base = 1

    real :: result1(n)
    integer :: result2(n)

    result1 = logspace(n, base)
    result2 = logspace(n, base)

    print *, result1
    print *, result2
    if (.not. all(result1 == [1.0, 2.0])) error stop
    if (.not. all(result2 == [1, 2])) error stop
end program