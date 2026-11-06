! Hosts whose internal procedure is an actual argument have a finalizable
! local that the declaration of another entity refers to: a local, an
! interface body that imports it, a dummy argument, and a namelist group.
! Each host compiles and its finalizable local is finalized once. GFortran
! evaluates inquiries about a local in a specification wrongly, so this
! test is not run with GFortran.
module finalization_55_mod
implicit none
integer :: finalized = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
    final :: finalize_scalar
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x(:)
    finalized = finalized + 1
end subroutine
subroutine finalize_scalar(x)
    type(t), intent(inout) :: x
    finalized = finalized + 1
end subroutine
subroutine apply(f, r)
    interface
        integer function f()
        end function
    end interface
    integer, intent(out) :: r
    r = f()
end subroutine
subroutine add2(a)
    integer, intent(inout) :: a(:)
    a = a + 2
end subroutine
recursive subroutine host_local(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    type(t) :: x(n)
    integer :: w(size(x, 1) + 1)
    w = 1
    call apply(get, r)
contains
    integer function get()
        get = sum(w)
    end function
end subroutine
subroutine host_import(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    type(t) :: x(n:2*n-1)
    interface
        subroutine inner(a)
            import :: x
            integer, intent(inout) :: a(lbound(x, 1):ubound(x, 1))
        end subroutine
    end interface
    integer :: b(n)
    b = 1
    call add2(b)
    call apply(get, r)
contains
    integer function get()
        get = sum(b)
    end function
end subroutine
subroutine host_dummy(n, y, r)
    integer, intent(in) :: n
    type(t) :: x(n:2*n-1)
    integer, intent(inout) :: y(lbound(x, 1):ubound(x, 1))
    integer, intent(out) :: r
    call apply(get, r)
contains
    integer function get()
        get = sum(y)
    end function
end subroutine
subroutine host_namelist(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    type(t) :: x
    integer :: k
    namelist /nl/ x, k
    x%v = n
    k = 3
    write(*, nml=nl)
    call apply(get, r)
contains
    integer function get()
        get = x%v + k
    end function
end subroutine
end module

program finalization_55
use finalization_55_mod
implicit none
integer :: r, y(3)
call host_local(3, r)
print *, r, finalized
if (r /= 4) error stop
if (finalized /= 1) error stop
call host_import(3, r)
print *, r, finalized
if (r /= 9) error stop
if (finalized /= 2) error stop
y = 2
call host_dummy(3, y, r)
print *, r, finalized
if (r /= 6) error stop
if (finalized /= 3) error stop
call host_namelist(3, r)
print *, r, finalized
if (r /= 6) error stop
if (finalized /= 4) error stop
end program
