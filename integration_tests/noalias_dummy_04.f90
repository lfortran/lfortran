! Type-bound procedures that update allocatable components of `self` from
! other dummy arguments (like the layers of neural-fortran), with noalias
! dummy arguments under --fast.
module noalias_dummy_04_m
implicit none
type :: layer_t
    real, allocatable :: w(:,:), dw(:,:), out(:)
contains
    procedure :: forward
    procedure :: backward
    procedure :: copy_from
end type
contains

subroutine forward(self, x)
    class(layer_t), intent(inout) :: self
    real, intent(in) :: x(:)
    integer :: i, j
    self%out = 0
    do j = 1, size(x)
        do i = 1, size(self%out)
            self%out(i) = self%out(i) + self%w(i,j)*x(j)
        end do
    end do
end subroutine

subroutine backward(self, x, g)
    class(layer_t), intent(inout) :: self
    real, intent(in) :: x(:), g(:)
    integer :: i, j
    do j = 1, size(x)
        do i = 1, size(g)
            self%dw(i,j) = self%dw(i,j) + g(i)*x(j)
        end do
    end do
end subroutine

subroutine copy_from(self, other)
    class(layer_t), intent(inout) :: self
    type(layer_t), intent(in) :: other
    self%w = other%w + 1
end subroutine

end module

program noalias_dummy_04
use noalias_dummy_04_m
implicit none
type(layer_t) :: l, m
real :: x(3)
integer :: i, j

allocate(l%w(2,3), l%dw(2,3), l%out(2))
do j = 1, 3
    do i = 1, 2
        l%w(i,j) = i + 10*j
    end do
end do
l%dw = 0
x = [1., 2., 3.]
call l%forward(x)
if (abs(l%out(1) - 146) > 1e-4) error stop 1
if (abs(l%out(2) - 152) > 1e-4) error stop 2

call l%backward(x, l%out)
if (abs(l%dw(1,1) - 146) > 1e-3) error stop 3
if (abs(l%dw(2,3) - 456) > 1e-3) error stop 4

! The component of `self` that the procedure reads through `g`, and the
! one it writes, are different.
call l%backward(l%out, l%out(1:2))
if (abs(l%dw(1,1) - (146 + 146*146)) > 1e-1) error stop 5

allocate(m%w(2,3))
m%w = 0
call m%copy_from(l)
if (abs(m%w(1,1) - 12) > 1e-5) error stop 6
if (abs(l%w(1,1) - 11) > 1e-5) error stop 7
print *, "ok"
end program
