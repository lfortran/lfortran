program separate_compilation_12
    use separate_compilation_12a_module

    implicit none
    integer :: a, b
    a = 1
    b = 2
    call parent_of_nested_subroutine(a, b)
    print *, "a =", a
    if ( a /= 2 ) error stop
    print *, "b =", b
    if ( b /= 4 ) error stop
end program
