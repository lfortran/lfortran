module template_local_type_02_tm
    implicit none

    template tm {n}
        deferred integer, parameter :: n
    contains
        integer function f() result(r)
            type :: loc
                integer :: a
            end type
            type(loc) :: s
            s = loc(2)
            r = g(s)
        contains
            integer function g(x) result(y)
                type(loc), intent(in) :: x
                y = x%a * n
            end function
        end function

        integer function h() result(r)
            integer :: i
            type :: loc
                integer :: a(n)
            end type
            type(loc) :: s
            s = loc([(2*i, i = 1, n)])
            r = sum(s%a)
        end function
    end template
end module

module template_local_type_02_inst
    use template_local_type_02_tm
    implicit none
    instantiate tm {3}, only: f3 => f, h3 => h
end module
