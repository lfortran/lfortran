module separate_compilation_56a
implicit none
contains
    elemental real function f(x)
        real, intent(in) :: x
        f = 2.0 * x
    end function

    elemental subroutine s(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine
end module
