module template_operator_scope_06_m
    ! A defined operator of the module must stay in the module, even though a
    ! requirement, a template and a templated function follow its interface.
    implicit none
    interface operator(.minus.)
        procedure sub
    end interface

    requirement binop {op}
        deferred interface
            pure integer function op(x, y)
                integer, intent(in) :: x, y
            end function
        end interface
    end requirement

    template twice_t {op}
        require :: binop {op}
    contains
        integer function twice(x, y) result(r)
            integer, intent(in) :: x, y
            r = op(op(x, y), y)
        end function
    end template
contains
    pure integer function sub(x, y)
        integer, intent(in) :: x, y
        sub = x - y
    end function

    template function apply {op}(x, y) result(r)
        deferred interface
            pure integer function op(x, y)
                integer, intent(in) :: x, y
            end function
        end interface
        integer, intent(in) :: x, y
        integer :: r
        r = op(x, y)
    end function
end module

program template_operator_scope_06
    use template_operator_scope_06_m
    implicit none
    instantiate twice_t {sub}, only: twice_sub => twice
    if ((9 .minus. 2) /= 7) error stop
    if (apply{sub}(9, 2) /= 7) error stop
    if (twice_sub(9, 2) /= 5) error stop
    print *, "ok"
end program
