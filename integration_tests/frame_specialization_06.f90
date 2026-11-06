! With --fast (#16665): `both` passes its paired dummy `f` on as a pair (or
! its own `nxt` with its frame) in the same call that passes `g`, which
! escapes to an external procedure and stays a raw procedure value. The
! dummy of `walk` has array, character and optional arguments, which are not
! paired: its internal procedure still gets thunks, so it recurses less
! deeply than --internal-procedure-thunks.
integer function apply_external(g, x)
implicit none
interface
    integer function g(y)
        integer, intent(in) :: y
    end function
end interface
integer, intent(in) :: x
apply_external = g(x)
end function

module frame_specialization_06_m
implicit none
interface
    integer function apply_external(g, x)
        interface
            integer function g(y)
                integer, intent(in) :: y
            end function
        end interface
        integer, intent(in) :: x
    end function
end interface
contains
    recursive integer function both(n, f, g) result(r)
        integer, intent(in) :: n
        interface
            integer function f(x)
                integer, intent(in) :: x
            end function
            integer function g(x)
                integer, intent(in) :: x
            end function
        end interface
        if (n == 0) then
            r = f(1) + apply_external(g, 2)
        else if (mod(n, 2) == 0) then
            r = both(n - 1, f, g)
        else
            r = both(n - 1, nxt, g)
        end if
    contains
        integer function nxt(x)
            integer, intent(in) :: x
            nxt = f(x) + n
        end function
    end function

    integer function base(x)
        integer, intent(in) :: x
        base = x
    end function

    integer function run(c) result(r)
        integer, intent(in) :: c
        r = both(30, base, scale)
    contains
        integer function scale(x)
            integer, intent(in) :: x
            scale = c*x
        end function
    end function

    recursive subroutine walk(n, s, acc)
        integer, intent(in) :: n
        integer, intent(inout) :: acc
        interface
            subroutine s(v, name, w, k, opt)
                integer, intent(inout) :: v(:)
                character(len=*), intent(in) :: name
                integer, intent(in) :: k
                integer, intent(inout) :: w(k)
                integer, intent(in), optional :: opt
            end subroutine
        end interface
        integer :: v(3), w(2)
        v = n
        w = 0
        if (n == 0) then
            call s(v, "end", w, 2)
            acc = acc + sum(v) + sum(w)
        else
            call walk(n - 1, inner, acc)
        end if
    contains
        subroutine inner(v, name, w, k, opt)
            integer, intent(inout) :: v(:)
            character(len=*), intent(in) :: name
            integer, intent(in) :: k
            integer, intent(inout) :: w(k)
            integer, intent(in), optional :: opt
            call s(v, name // "x", w, k, n)
            v = v + len(name)
            if (present(opt)) w = w + opt
        end subroutine
    end subroutine

    subroutine leaf(v, name, w, k, opt)
        integer, intent(inout) :: v(:)
        character(len=*), intent(in) :: name
        integer, intent(in) :: k
        integer, intent(inout) :: w(k)
        integer, intent(in), optional :: opt
        v = v + 1
        if (present(opt)) w = w + opt
    end subroutine
end module

program frame_specialization_06
use frame_specialization_06_m
implicit none
integer :: acc
if (run(3) /= 232) error stop 1
acc = 0
call walk(5, leaf, acc)
if (acc /= 108) error stop 2
print *, acc
end program
