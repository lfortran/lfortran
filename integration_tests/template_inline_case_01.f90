module template_inline_case_01_m
    implicit none
contains
    template function g{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        type(t) :: g
        g = x
    end function

    template subroutine s{t}(x, y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine
end module

program template_inline_case_01
    use template_inline_case_01_m
    implicit none
    integer :: i, j
    i = G{integer}(5)
    print *, i
    if (i /= 5) error stop
    call S{integer}(7, j)
    print *, j
    if (j /= 7) error stop
end program
