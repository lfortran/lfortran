! A specification expression calls a generic interface whose specific
! procedures are module procedures defined later.
module spec_expr_15_mod
implicit none
interface gsize
    module procedure gsize_i, gsize_r
end interface
interface area
    module procedure area_i, area_r
end interface
interface norm
    module procedure norm, norm_r
end interface
contains
    function f(n) result(r)
        integer, intent(in) :: n
        integer :: r(gsize(n))
        r = 3
    end function f

    pure integer function gsize_i(n)
        integer, intent(in) :: n
        gsize_i = n + 1
    end function gsize_i

    pure integer function gsize_r(x)
        real, intent(in) :: x
        gsize_r = int(x)
    end function gsize_r

    ! The signature of a specific procedure calls its own generic
    ! interface, which resolves to the other specific procedure.
    pure function area_i(n) result(r)
        integer, intent(in) :: n
        integer :: r(area(real(n)))
        r = 1
    end function area_i

    pure integer function area_r(x)
        real, intent(in) :: x
        area_r = int(x) + 1
    end function area_r

    ! A specific procedure has the name of its generic interface.
    function g(n) result(r)
        integer, intent(in) :: n
        integer :: r(norm(n))
        r = 0
    end function g

    pure integer function norm(n)
        integer, intent(in) :: n
        norm = n + 1
    end function norm

    pure integer function norm_r(x)
        real, intent(in) :: x
        norm_r = int(x) + 2
    end function norm_r
end module spec_expr_15_mod

program spec_expr_15
use spec_expr_15_mod
implicit none
if (size(f(4)) /= 5) error stop
if (any(f(4) /= 3)) error stop
if (size(area(4)) /= 5) error stop
if (size(g(3)) /= 4) error stop
if (norm(2.0) /= 4) error stop
print *, size(f(4)), size(area(4)), size(g(3)), norm(2.0)
end program spec_expr_15
