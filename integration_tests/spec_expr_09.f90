program spec_expr_09
    ! A result bound and a result length that call a dummy procedure
    ! of the function.
    implicit none
    interface
        pure integer function spec_expr_09_f(n)
            integer, intent(in) :: n
        end function
    end interface
    integer :: a(6)
    a = h(spec_expr_09_f, 3)
    print *, size(h(spec_expr_09_f, 3)), sum(a)
    if (size(h(spec_expr_09_f, 3)) /= 6) error stop
    if (sum(a) /= 6) error stop
    print *, len(s(spec_expr_09_f, 2)), s(spec_expr_09_f, 2)
    if (len(s(spec_expr_09_f, 2)) /= 4) error stop
    if (s(spec_expr_09_f, 2) /= "abcd") error stop
contains
    function h(fp, n) result(r)
        interface
            pure integer function fp(n)
                integer, intent(in) :: n
            end function
        end interface
        integer, intent(in) :: n
        integer :: r(fp(n))
        r = 1
    end function
    function s(fp, n) result(r)
        interface
            pure integer function fp(n)
                integer, intent(in) :: n
            end function
        end interface
        integer, intent(in) :: n
        character(len=fp(n)) :: r
        r = "abcd"
    end function
end program

pure integer function spec_expr_09_f(n)
    integer, intent(in) :: n
    spec_expr_09_f = 2*n
end function
