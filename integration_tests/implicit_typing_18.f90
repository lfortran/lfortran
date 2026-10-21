! The implicit mapping of a host applies in the host after its CONTAINS
! section and in its internal procedures, where an IMPLICIT statement of the
! procedure overrides it only for the letters it names.
module implicit_typing_18_mod
implicit double precision (a-h, o-z)
contains
    function third(y)
        third = half(y) * 2d0 / 3d0
        r = third
        if (kind(r) /= 8) error stop
    contains
        function half(z)
            half = z / 2d0
            d = half
            if (kind(d) /= 8) error stop
        end function
    end function

    subroutine own_mapping(n, e)
        implicit integer (d)
        d = 2
        e = 1d0 / 3d0
        n = d
        if (kind(d) /= 4) error stop
        if (kind(e) /= 8) error stop
    end subroutine
end module

subroutine external_host(res)
implicit double precision (a-h, o-z)
r = 1d0 / 3d0
call inner(d)
res = r + d
if (kind(r) /= 8) error stop
contains
    subroutine inner(x)
        double precision, intent(out) :: x
        e = 1d0 / 3d0
        x = e
        if (kind(e) /= 8) error stop
    end subroutine
end subroutine

program implicit_typing_18
use implicit_typing_18_mod
implicit double precision (a-h, o-z)
r = 1d0 / 3d0
if (kind(r) /= 8) error stop
if (abs(r - 1d0 / 3d0) > 1d-15) error stop
call set_third(x)
if (abs(x - 1d0 / 3d0) > 1d-15) error stop
call internal_local(y)
if (abs(y - 1d0 / 3d0) > 1d-15) error stop
call own_mapping(n, e)
if (n /= 2) error stop
if (abs(e - 1d0 / 3d0) > 1d-15) error stop
call partial_override(n, e)
if (n /= 3) error stop
if (abs(e - 1d0 / 3d0) > 1d-15) error stop
w = third(1d0)
if (abs(w - 1d0 / 3d0) > 1d-15) error stop
call external_host(v)
if (abs(v - 2d0 / 3d0) > 1d-15) error stop
print *, r, x, y, e, w, v
contains
    subroutine set_third(z)
        double precision, intent(out) :: z
        z = 1d0 / 3d0
    end subroutine

    subroutine internal_local(z)
        double precision, intent(out) :: z
        d = 1d0 / 3d0
        z = d
        if (kind(d) /= 8) error stop
    end subroutine

    subroutine partial_override(m, f)
        implicit integer (m)
        m = 3
        f = 1d0 / 3d0
        if (kind(f) /= 8) error stop
    end subroutine
end program
