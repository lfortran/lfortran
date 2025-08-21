module tester_separate_compilation_24
    use sorting_separate_compilation_24
    implicit none
end module tester_separate_compilation_24

program separate_compilation_24
    use tester_separate_compilation_24

    implicit none

    integer :: n = 1
    call sort( n )

    print *, n
    if (n /= 5) error stop
end program