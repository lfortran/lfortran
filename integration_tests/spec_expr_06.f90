program spec_expr_06
    ! A generic of the main program referenced in an array bound of one of
    ! its internal procedures.
    implicit none
    interface g
        pure integer function spec_expr_06_f(n)
            integer, intent(in) :: n
        end function
    end interface
    integer :: r
    call s(3, r)
    print *, r
    if (r /= 6) error stop
contains
    subroutine s(n, r)
        integer, intent(in) :: n
        integer, intent(out) :: r
        integer :: arr(g(n))
        arr = 1
        r = sum(arr)
    end subroutine
end program

pure integer function spec_expr_06_f(n)
    integer, intent(in) :: n
    spec_expr_06_f = 2*n
end function
