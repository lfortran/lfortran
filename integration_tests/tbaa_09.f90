! A C address stored in a c_ptr component is converted with TRANSFER to
! integer(c_intptr_t), kept as integer array data, converted back and used
! through C_F_POINTER inside loops that also store to the integer copies and
! to the data through other names; a pointer component is re-associated from
! such a reconstructed address inside the loop.
module tbaa_09_m
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_intptr_t
implicit none

type :: rec_t
    type(c_ptr) :: addr
    real, pointer :: v(:) => null()
    integer(c_intptr_t), allocatable :: key(:)
end type

contains

subroutine roundtrip(self, a, keys, n)
    type(rec_t), intent(inout) :: self
    integer, intent(in) :: n
    real, allocatable, target, intent(inout) :: a(:)
    integer(c_intptr_t), allocatable, intent(inout) :: keys(:)
    real, pointer :: p(:), q(:)
    type(c_ptr) :: cp
    integer(c_intptr_t) :: k
    integer :: i
    self%addr = c_loc(a)
    do i = 1, n
        call c_f_pointer(self%addr, p, [n])
        p(i) = p(i) + i
        k = transfer(self%addr, k)
        keys(i) = k
        a(i) = 2*a(i)
        keys(i) = keys(i) - 7
        cp = transfer(keys(i) + 7, cp)
        call c_f_pointer(cp, q, [n])
        q(i) = q(i) + a(i)
        keys(i) = keys(i) + 7
        self%addr = transfer(keys(i), self%addr)
        a(i) = a(i) + p(i) + q(i)
    end do
end subroutine

subroutine rotate(self, a, b, n)
    type(rec_t), intent(inout) :: self
    integer, intent(in) :: n
    real, target, intent(inout) :: a(n), b(n)
    integer(c_intptr_t) :: k
    integer :: i
    k = transfer(c_loc(a), k)
    self%key(1) = k
    k = transfer(c_loc(b), k)
    self%key(2) = k
    do i = 1, n
        k = self%key(1)
        self%key(1) = self%key(2)
        self%key(2) = k
        self%addr = transfer(self%key(1), self%addr)
        call c_f_pointer(self%addr, self%v, [n])
        self%v(i) = self%v(i) + 10*i
        a(i) = a(i) + 1
        b(i) = b(i) + self%v(i)
    end do
end subroutine

end module

program tbaa_09
use tbaa_09_m
implicit none
type(rec_t) :: r
real, allocatable, target :: a(:)
integer(c_intptr_t), allocatable :: keys(:)
real, target :: x(4), y(4)
integer(c_intptr_t) :: k
integer :: i

allocate(a(5), keys(5))
a = 1
keys = 0
call roundtrip(r, a, keys, 5)
print *, a
! p(i) = 1 + i, a(i) = 2 + 2i, q(i) = 4 + 4i, a(i) = 3*(4 + 4i)
do i = 1, 5
    if (abs(a(i) - 12*(1 + i)) > 1e-6) error stop 1
end do
if (any(keys /= keys(1))) error stop 2
k = transfer(c_loc(a), k)
if (keys(1) /= k) error stop 3

allocate(r%key(2))
x = 0
y = 0
call rotate(r, x, y, 4)
print *, x
print *, y
if (any(abs(x - [1.0, 21.0, 1.0, 41.0]) > 1e-6)) error stop 4
if (any(abs(y - [20.0, 21.0, 60.0, 41.0]) > 1e-6)) error stop 5
k = transfer(c_loc(x), k)
if (r%key(1) /= k) error stop 6
k = transfer(c_loc(y), k)
if (r%key(2) /= k) error stop 7
if (.not. associated(r%v)) error stop 8
if (abs(r%v(4) - 41.0) > 1e-6) error stop 9
print *, "ok"
end program
