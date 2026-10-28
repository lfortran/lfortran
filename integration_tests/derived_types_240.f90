! An array component operand of an elementwise operation that a pointer can
! designate, or that can designate the assignment target, is copied before
! the target is defined (#16215)
! Not labelled gfortran: GFortran 16.2 miscompiles these cases; for example,
! with a%p => c, `c(3:1:-1) = a%p + 1` gives [3, 3, 2] instead of [4, 3, 2]
! because it reads a%p while c is being defined. Flang agrees with LFortran.
program derived_types_240
implicit none
type :: t
    real, allocatable :: x(:)
    real, pointer :: p(:) => null()
end type
type(t) :: a
type(t), target :: tg
type(t), pointer :: pa
real, target :: c(3)
real, pointer :: q(:)

! A pointer component can designate the target.
c = [1., 2., 3.]
a%p => c(3:1:-1)
c = a%p + 1
if (any(c /= [4., 3., 2.])) error stop 1
c = [1., 2., 3.]
a%p => c
c(3:1:-1) = a%p * 2
if (any(c /= [6., 4., 2.])) error stop 2
c = [1., 2., 3.]
a%p => c(3:1:-1)
c = -a%p
if (any(c /= [-3., -2., -1.])) error stop 3

! A pointer can designate a component of a target.
allocate(tg%x(3))
tg%x = [1., 2., 3.]
q => tg%x(3:1:-1)
q = tg%x + 1
if (any(tg%x /= [4., 3., 2.])) error stop 4

! A component of a pointer or of an associate name.
tg%x = [1., 2., 3.]
pa => tg
q = pa%x + 1
if (any(tg%x /= [4., 3., 2.])) error stop 5
tg%x = [1., 2., 3.]
associate (r => tg)
    q = r%x * 2
end associate
if (any(tg%x /= [6., 4., 2.])) error stop 6
print *, "ok"
end program
