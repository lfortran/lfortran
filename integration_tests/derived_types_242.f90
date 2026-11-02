! The same component of an associate name and of its selector overlaps, so
! it is copied before the target is defined (#16215)
! Not labelled gfortran: GFortran 16.2 miscompiles these cases; for example,
! `associate (b => c)` followed by `c%x(2:3) = b%x(1:2)` gives [1, 1, 1]
! instead of [1, 1, 2] because it reads b%x while c%x is being defined.
! Flang agrees with LFortran.
program derived_types_242
implicit none
type :: t
    real, allocatable :: x(:)
    real :: w(3)
end type
type(t) :: c

allocate(c%x(3))
c%x = [1., 2., 3.]
associate (b => c)
    c%x(2:3) = b%x(1:2)
end associate
if (any(c%x /= [1., 1., 2.])) error stop 1

c%x = [1., 10., 100.]
associate (b => c)
    b%x(2:3) = c%x(1:2) + 1
end associate
if (any(c%x /= [1., 2., 11.])) error stop 2

c%w = [1., 2., 3.]
associate (b => c)
    b%w = c%w(3:1:-1)
end associate
if (any(c%w /= [3., 2., 1.])) error stop 3

c%x = [1., 2., 3.]
call shift(c)
if (any(c%x /= [1., 1., 2.])) error stop 4
print *, "ok"

contains

subroutine shift(d)
    type(t), intent(inout) :: d
    associate (e => d%x)
        d%x(2:3) = e(1:2)
    end associate
end subroutine

end program
