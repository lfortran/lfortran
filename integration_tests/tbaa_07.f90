! C_F_POINTER to memory that the loop also accesses as a Fortran array,
! with the C address and the pointer stored in a derived type.
module tbaa_07_m
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
implicit none

type :: raw_t
    type(c_ptr) :: addr
    real, pointer :: v(:) => null()
end type

contains

subroutine smooth(a, n)
    integer, intent(in) :: n
    real, target, intent(inout) :: a(n)
    real, pointer :: p(:)
    integer :: i
    call c_f_pointer(c_loc(a), p, [n])
    do i = 2, n
        p(i) = a(i-1) + p(i)
    end do
end subroutine

subroutine through_component(self, a, n)
    type(raw_t), intent(inout) :: self
    integer, intent(in) :: n
    real, allocatable, target, intent(inout) :: a(:)
    integer :: i
    self%addr = c_loc(a)
    do i = 2, n
        call c_f_pointer(self%addr, self%v, [n])
        self%v(i) = self%v(i) + a(i-1)
        a(i-1) = 0
    end do
end subroutine

end module

program tbaa_07
use tbaa_07_m
implicit none
real, target :: a(5)
real, allocatable, target :: b(:)
type(raw_t) :: r
a = 1
call smooth(a, 5)
if (any(abs(a - [1.0, 2.0, 3.0, 4.0, 5.0]) > 1e-6)) error stop 1
allocate(b(4))
b = 1
call through_component(r, b, 4)
print *, b
if (any(abs(b - [0.0, 0.0, 0.0, 4.0]) > 1e-6)) error stop 2
print *, "ok"
end program
