! A recursive host passes its internal procedure to a module procedure in
! every instance while deeper instances are active. Without the copy of the
! module procedure in the host that --fast makes, each instance would hold a
! thunk of the internal procedure, and the test is registered with
! --internal-procedure-thunks=2, so it would stop at the third instance.
module procedure_argument_specialization_02_m
implicit none
contains
integer function apply(f, x) result(r)
    interface
        integer function f(y)
            integer, intent(in) :: y
        end function
    end interface
    integer, intent(in) :: x
    r = f(x)
end function

recursive integer function host(depth) result(r)
    integer, intent(in) :: depth
    r = apply(add_depth, 1)
    if (depth > 1) r = r + host(depth - 1)
    r = r + apply(add_depth, 0)
contains
    integer function add_depth(y)
        integer, intent(in) :: y
        add_depth = y + depth
    end function
end function
end module

program procedure_argument_specialization_02
use procedure_argument_specialization_02_m, only: host
implicit none
integer :: r
r = host(10)
! sum over depth = 1..10 of (1 + depth) + depth
if (r /= 120) error stop 1
print *, r
end program
