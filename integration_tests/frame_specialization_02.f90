! With --fast (#16665), `chain` gets a copy that takes its dummy procedure
! `f` with the frame of its host, so that each of its recursive instances
! passes its internal procedure `next` on without a thunk. Its other dummy
! procedure `g` escapes, to an external procedure (with no body that the
! module sees) and to c_funloc, so it stays a raw procedure value: the internal procedure
! `scale` that `run` passes to it still gets a thunk.
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

module frame_specialization_02_m
use iso_c_binding, only: c_funptr, c_funloc, c_f_procpointer
implicit none
abstract interface
    integer function int_function(x)
        integer, intent(in) :: x
    end function
end interface
interface
    integer function apply_external(g, x)
        import :: int_function
        procedure(int_function) :: g
        integer, intent(in) :: x
    end function
end interface
contains
    recursive integer function chain(n, f, g) result(r)
        integer, intent(in) :: n
        procedure(int_function) :: f, g
        type(c_funptr) :: p
        procedure(int_function), pointer :: q
        if (n == 0) then
            p = c_funloc(g)
            call c_f_procpointer(p, q)
            r = apply_external(g, f(0)) + q(1)
        else
            r = chain(n - 1, next, g)
        end if
    contains
        integer function next(x)
            integer, intent(in) :: x
            next = f(x) + n
        end function
    end function

    integer function base(x)
        integer, intent(in) :: x
        base = x + 1
    end function

    integer function run(c, depth) result(r)
        integer, intent(in) :: c, depth
        r = chain(depth, base, scale)
    contains
        integer function scale(x)
            integer, intent(in) :: x
            scale = c*x
        end function
    end function
end module

program frame_specialization_02
use frame_specialization_02_m, only: run
implicit none
integer :: r
! f(0) = 1 + (1 + 2 + ... + 30) = 466, then 3*466 + 3*1
r = run(3, 30)
if (r /= 1401) error stop 1
r = run(2, 5)
if (r /= 2*16 + 2) error stop 2
print *, r
end program
