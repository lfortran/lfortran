program spec_expr_07
    ! An interface-body function of the main program in the result bound
    ! and in the result length of its internal functions.
    implicit none
    interface
        pure integer function spec_expr_07_f(n)
            integer, intent(in) :: n
        end function
    end interface
    integer :: a(6)
    a = h(3)
    print *, sum(a)
    if (sum(a) /= 6) error stop
    call k()
contains
    function h(n) result(r)
        integer, intent(in) :: n
        integer :: r(spec_expr_07_f(n))
        r = 1
    end function
    function s(n) result(r)
        integer, intent(in) :: n
        character(len=spec_expr_07_f(n)) :: r
        r = "ab"
    end function
    subroutine k()
        character(len=6) :: c
        c = s(3)
        print *, len(s(3)), c
        if (len(s(3)) /= 6) error stop
        if (c /= "ab") error stop
    end subroutine
end program

pure integer function spec_expr_07_f(n)
    integer, intent(in) :: n
    spec_expr_07_f = 2*n
end function
