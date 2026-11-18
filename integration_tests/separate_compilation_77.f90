! External and module procedures, a module variable and a type-bound
! procedure defined in units compiled separately with --all-mangling link
! with the units that reference them (#17306, #16381)
program separate_compilation_77
    use separate_compilation_77a, only: twice, counter, shape
    implicit none
    interface
        subroutine separate_compilation_77_ext(x)
            integer, intent(out) :: x
        end subroutine

        integer function separate_compilation_77_add(a, b)
            integer, intent(in) :: a, b
        end function
    end interface
    integer :: i
    type(shape) :: s
    call separate_compilation_77_ext(i)
    if (i /= 42) error stop
    if (twice(5) /= 10) error stop
    if (counter /= 2) error stop
    if (separate_compilation_77_add(40, 2) /= 42) error stop
    s%n = 3
    if (s%area() /= 9) error stop
    print *, i, counter
end program
