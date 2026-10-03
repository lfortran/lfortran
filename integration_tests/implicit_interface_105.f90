! Rows of arrays passed to dummy procedures and procedure pointers called
! through an implicit interface are copied back after the call: `external`
! and `procedure()` dummies, a `procedure()` pointer and a pointer with an
! abstract interface associated with an external subroutine.
module implicit_interface_105_m
implicit none
abstract interface
    subroutine row_iface(v, n)
        integer :: n
        real(8) :: v(n)
    end subroutine
end interface
contains
subroutine drive_assumed(w, s)
    real(8), intent(inout) :: w(:,:)
    external s
    integer :: i
    do i = 1, size(w, 1)
        call s(w(i, :), size(w, 2))
    end do
end subroutine
end module

subroutine implicit_interface_105_incr(v, n)
integer :: n
real(8) :: v(n)
v = v + 1
end subroutine

subroutine implicit_interface_105_drive_explicit(w, s)
real(8), intent(inout) :: w(2, 3)
external s
integer :: i
do i = 1, 2
    call s(w(i, :), 3)
end do
end subroutine

subroutine implicit_interface_105_drive_procedure(w, s)
real(8), intent(inout) :: w(2, 3)
procedure() :: s
integer :: i
do i = 1, 2
    call s(w(i, :), 3)
end do
end subroutine

program implicit_interface_105
use implicit_interface_105_m
implicit none
external implicit_interface_105_incr
external implicit_interface_105_drive_explicit
external implicit_interface_105_drive_procedure
procedure(), pointer :: p
procedure(row_iface), pointer :: q
real(8) :: w(2, 3)
real(8), allocatable :: a(:,:)
integer :: i

w = 0
call drive_assumed(w, implicit_interface_105_incr)
print *, sum(w)
if (abs(sum(w) - 6d0) > 1d-12) error stop 1

w = 0
call implicit_interface_105_drive_explicit(w, implicit_interface_105_incr)
print *, sum(w)
if (abs(sum(w) - 6d0) > 1d-12) error stop 2

w = 0
call implicit_interface_105_drive_procedure(w, implicit_interface_105_incr)
print *, sum(w)
if (abs(sum(w) - 6d0) > 1d-12) error stop 3

allocate(a(2, 3))
a = 0
p => implicit_interface_105_incr
do i = 1, 2
    call p(a(i, :), 3)
end do
print *, sum(a)
if (abs(sum(a) - 6d0) > 1d-12) error stop 4

a = 0
q => implicit_interface_105_incr
do i = 1, 2
    call q(a(i, :), 3)
end do
print *, sum(a)
if (abs(sum(a) - 6d0) > 1d-12) error stop 5
end program
