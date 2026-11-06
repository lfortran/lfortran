! The same component of two different variables that no pointer can reach,
! such as `c%x` and `a%x` of two dummy arguments, does not overlap, so it is
! read in place; components of one variable, or of variables a pointer can
! reach, are still copied when they may overlap the target (#16215)
module derived_types_241_m
implicit none
type :: t
    real, allocatable :: x(:), z(:,:)
    real :: w(3)
end type
type :: outer
    type(t) :: in
end type
type(t) :: g
type(t), target :: gt
contains

subroutine sec_add(a, c, n)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    integer, intent(in) :: n
    c%x(1:n) = a%x(1:n) + a%z(1:n,1)
end subroutine

subroutine sec_copy(a, c, n)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    integer, intent(in) :: n
    c%x(1:n) = a%x(1:n)
end subroutine

subroutine sec_shift(a, c)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    c%x(2:3) = a%x(1:2)
end subroutine

subroutine whole_copy(a, c)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    c%x = a%x
end subroutine

subroutine fixed_add(a, c)
    type(t), intent(in) :: a
    type(t), intent(out) :: c
    c%w = a%w + 1
end subroutine

subroutine fixed_sec(a, c)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    c%w(1:2) = a%w(2:3) + 1
end subroutine

subroutine nested(a, c)
    type(outer), intent(in) :: a
    type(outer), intent(inout) :: c
    c%in%x = a%in%x * 2
end subroutine

subroutine to_module(c)
    type(t), intent(in) :: c
    g%x = c%x + 1
end subroutine

subroutine from_module(c)
    type(t), intent(inout) :: c
    c%x(1:2) = g%x(2:3)
end subroutine

subroutine elements(u, v, i, j)
    type(t), intent(in) :: u(:)
    type(t), intent(inout) :: v(:)
    integer, intent(in) :: i, j
    v(i)%x(2:3) = u(j)%x(1:2) - 1
end subroutine

! Fallbacks: the target and the operand may overlap

subroutine same_object(c)
    type(t), intent(inout) :: c
    c%x(2:3) = c%x(1:2)
end subroutine

subroutine same_array(v, i, j)
    type(t), intent(inout) :: v(:)
    integer, intent(in) :: i, j
    v(i)%x(2:3) = v(j)%x(1:2) * 10
end subroutine

subroutine same_array_fixed(v, i, j)
    type(t), intent(inout) :: v(:)
    integer, intent(in) :: i, j
    v(i)%w = v(j)%w(3:1:-1)
end subroutine

! TARGET dummies may be associated with the same actual argument.
subroutine targets(a, c)
    type(t), target, intent(inout) :: a, c
    c%x(2:3) = a%x(1:2) + 1
end subroutine

subroutine targets_fixed(a, c)
    type(t), target, intent(inout) :: a, c
    c%w = a%w(3:1:-1)
end subroutine

subroutine through_pointer(p, c)
    type(t), pointer, intent(in) :: p
    type(t), target, intent(inout) :: c
    c%x(2:3) = p%x(1:2)
end subroutine

subroutine module_pointer()
    type(t), pointer :: p
    p => gt
    gt%x(2:3) = p%x(1:2) + 1
end subroutine

end module

program derived_types_241
use derived_types_241_m
implicit none
type(t) :: a, c, u(2), v(2)
type(t), target :: tg
type(t), pointer :: p
type(outer) :: oa, oc

allocate(a%x(3), a%z(3,1))
a%x = [1., 2., 3.]
a%z = reshape([10., 20., 30.], [3, 1])
a%w = [4., 5., 6.]

call fixed_add(a, c)
if (any(c%w /= [5., 6., 7.])) error stop 1
call fixed_sec(a, c)
if (any(c%w /= [6., 7., 7.])) error stop 2

allocate(c%x(3))
c%x = 0
call sec_add(a, c, 2)
if (any(c%x /= [11., 22., 0.])) error stop 3
call sec_copy(a, c, 1)
if (any(c%x /= [1., 22., 0.])) error stop 4
call sec_shift(a, c)
if (any(c%x /= [1., 1., 2.])) error stop 5
call whole_copy(a, c)
if (any(c%x /= [1., 2., 3.])) error stop 6

allocate(oa%in%x(2), oc%in%x(2))
oa%in%x = [3., 4.]
call nested(oa, oc)
if (any(oc%in%x /= [6., 8.])) error stop 7

allocate(g%x(3))
call to_module(c)
if (any(g%x /= [2., 3., 4.])) error stop 8
call from_module(c)
if (any(c%x /= [3., 4., 3.])) error stop 9

allocate(u(1)%x(3), u(2)%x(3), v(1)%x(3), v(2)%x(3))
u(1)%x = [1., 2., 3.]
u(2)%x = [4., 5., 6.]
v(1)%x = 0
v(2)%x = 0
call elements(u, v, 2, 2)
if (any(v(2)%x /= [0., 3., 4.])) error stop 10
call elements(u, v, 1, 2)
if (any(v(1)%x /= [0., 3., 4.])) error stop 11

c%x = [1., 2., 3.]
call same_object(c)
if (any(c%x /= [1., 1., 2.])) error stop 12

v(1)%x = [1., 2., 3.]
v(2)%x = [4., 5., 6.]
call same_array(v, 1, 1)
if (any(v(1)%x /= [1., 10., 20.])) error stop 13
call same_array(v, 2, 1)
if (any(v(2)%x /= [4., 10., 100.])) error stop 14

v(1)%w = [1., 2., 3.]
v(2)%w = [4., 5., 6.]
call same_array_fixed(v, 1, 1)
if (any(v(1)%w /= [3., 2., 1.])) error stop 15
call same_array_fixed(v, 2, 1)
if (any(v(2)%w /= [1., 2., 3.])) error stop 16

allocate(tg%x(3))
tg%x = [1., 10., 100.]
call targets(tg, tg)
if (any(tg%x /= [1., 2., 11.])) error stop 17
tg%w = [1., 2., 3.]
call targets_fixed(tg, tg)
if (any(tg%w /= [3., 2., 1.])) error stop 18

p => tg
tg%x = [1., 2., 3.]
tg%x(2:3) = p%x(1:2)
if (any(tg%x /= [1., 1., 2.])) error stop 19
tg%x = [1., 2., 3.]
tg%x = p%x(3:1:-1)
if (any(tg%x /= [3., 2., 1.])) error stop 20
tg%w = [1., 2., 3.]
tg%w = p%w(3:1:-1)
if (any(tg%w /= [3., 2., 1.])) error stop 21
tg%x = [1., 2., 3.]
call through_pointer(p, tg)
if (any(tg%x /= [1., 1., 2.])) error stop 22
allocate(gt%x(3))
gt%x = [1., 10., 100.]
call module_pointer()
if (any(gt%x /= [1., 2., 11.])) error stop 23

print *, "ok"
end program
