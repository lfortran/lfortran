module procedure_argument_specialization_02_m
implicit none

contains

integer(4) function apply(f, x) result(r)
    integer(4), intent(in) :: x
    interface
        integer(4) function f(y)
            integer(4), intent(in) :: y
        end function f

    end interface
    r = f(x)
end function apply

integer(4) function host(depth) result(r)
    integer(4), intent(in) :: depth
    r = apply__add_depth(1)
    if (depth > 1) then
        r = r + host(depth - 1)
    end if
    r = r + apply__add_depth(0)
    contains
    integer(4) function add_depth(y)
        integer(4), intent(in) :: y
        add_depth = y + depth
    end function add_depth

    integer(4) function apply__add_depth(x) result(r)
        integer(4), intent(in) :: x
        r = add_depth(x)
    end function apply__add_depth

end function host

end module procedure_argument_specialization_02_m

program procedure_argument_specialization_02
use procedure_argument_specialization_02_m, only: host
implicit none
integer(4) :: r
r = host(10)
if (r /= 120) then
    error stop
end if
print *, r
end program procedure_argument_specialization_02
