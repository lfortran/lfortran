! Writes through SELECT TYPE on unlimited polymorphic dummy arguments and
! components inside loops.
module tbaa_08_m
implicit none

type :: box_t
    class(*), allocatable :: v
    class(*), allocatable :: arr(:)
end type

contains

subroutine poly_array(x)
    class(*), intent(inout) :: x(:)
    integer :: i
    do i = 1, size(x)
        select type (x)
        type is (real)
            x(i) = x(i) + i
        type is (integer)
            x(i) = x(i)*2 + x(1)
        end select
    end do
end subroutine

subroutine poly_component(self, n)
    type(box_t), intent(inout) :: self
    integer, intent(in) :: n
    integer :: i
    do i = 1, n
        select type (v => self%v)
        type is (integer)
            v = v + i
        end select
        select type (a => self%arr)
        type is (real)
            a(mod(i, size(a)) + 1) = a(mod(i, size(a)) + 1) + i
        end select
    end do
    self%v = 1.5
end subroutine

end module

program tbaa_08
use tbaa_08_m
implicit none
real :: r(3)
integer :: k(3)
type(box_t) :: b
r = 1
call poly_array(r)
if (any(abs(r - [2.0, 3.0, 4.0]) > 1e-6)) error stop 1
k = 1
call poly_array(k)
print *, k
if (any(k /= [3, 5, 5])) error stop 2
allocate(b%v, source=10)
allocate(b%arr, source=[0.0, 0.0, 0.0])
call poly_component(b, 4)
select type (a => b%arr)
type is (real)
    print *, a
    if (any(abs(a - [3.0, 5.0, 2.0]) > 1e-6)) error stop 3
class default
    error stop 4
end select
select type (v => b%v)
type is (real)
    if (abs(v - 1.5) > 1e-6) error stop 5
class default
    error stop 6
end select
print *, "ok"
end program
