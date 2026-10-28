! The result variables of a function and of its entries are the same
! variable (F2018 15.6.2.6), so the same component of two of them overlaps
! and is copied before the target is defined (#16215)
! Not labelled gfortran: GFortran 16.2 miscompiles these cases;
! `r%w = s%w(3:1:-1)` gives [3, 2, 3] instead of [3, 2, 1] because it reads
! s%w while r%w is being defined. Flang agrees with LFortran.
module entry_31_m
implicit none
type t
    real :: w(3)
    real, allocatable :: x(:)
end type

contains

function f() result(r)
    type(t) :: r
    type(t) :: s
    entry g() result(s)
    s%w = [1., 2., 3.]
    r%w = s%w(3:1:-1)
end function

function h() result(r)
    type(t) :: r
    type(t) :: s
    entry k() result(s)
    allocate(s%x(3))
    s%x = [1., 10., 100.]
    r%x(2:3) = s%x(1:2) + 1
end function

end module

function ef() result(r)
    use entry_31_m, only: t
    implicit none
    type(t) :: r
    type(t) :: s
    entry eg() result(s)
    s%w = [1., 2., 3.]
    r%w = s%w(3:1:-1)
end function

program entry_31
use entry_31_m
implicit none
interface
    function ef() result(r)
        import t
        type(t) :: r
    end function
    function eg() result(s)
        import t
        type(t) :: s
    end function
end interface
type(t) :: y

y = f()
if (any(y%w /= [3., 2., 1.])) error stop 1
y = g()
if (any(y%w /= [3., 2., 1.])) error stop 2
y = h()
if (any(y%x /= [1., 2., 11.])) error stop 3
y = k()
if (any(y%x /= [1., 2., 11.])) error stop 4
y = ef()
if (any(y%w /= [3., 2., 1.])) error stop 5
y = eg()
if (any(y%w /= [3., 2., 1.])) error stop 6
print *, "ok"
end program
