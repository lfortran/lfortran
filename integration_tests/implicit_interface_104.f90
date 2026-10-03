! Rows of arrays passed to an external procedure called through an implicit
! interface are copied back after the call: an allocatable array of the main
! program, an explicit-shape dummy, a pointer array and a local allocatable
! array of a module procedure.
module implicit_interface_104_m
implicit none
contains
subroutine work(total)
    real(8), intent(out) :: total
    external implicit_interface_104_incr
    real(8), allocatable :: w(:,:)
    integer :: i
    allocate(w(3, 2))
    w = 0
    do i = 1, 3
        call implicit_interface_104_incr(w(i, :), 2)
    end do
    total = sum(w)
end subroutine
end module

subroutine implicit_interface_104_incr(v, n)
integer :: n
real(8) :: v(n)
v = v + 1
end subroutine

subroutine implicit_interface_104_drive(w)
real(8), intent(inout) :: w(2, 3)
external implicit_interface_104_incr
integer :: i
do i = 1, 2
    call implicit_interface_104_incr(w(i, :), 3)
end do
end subroutine

program implicit_interface_104
use implicit_interface_104_m
implicit none
external implicit_interface_104_incr, implicit_interface_104_drive
real(8), allocatable :: a(:,:)
real(8), pointer :: p(:,:)
real(8) :: e(2, 3), total
integer :: i

allocate(a(2, 3))
a = 0
do i = 1, 2
    call implicit_interface_104_incr(a(i, :), 3)
end do
print *, sum(a)
if (abs(sum(a) - 6d0) > 1d-12) error stop 1

e = 0
call implicit_interface_104_drive(e)
print *, sum(e)
if (abs(sum(e) - 6d0) > 1d-12) error stop 2

allocate(p(2, 3))
p = 0
do i = 1, 2
    call implicit_interface_104_incr(p(i, :), 3)
end do
print *, sum(p)
if (abs(sum(p) - 6d0) > 1d-12) error stop 3
deallocate(p)

call work(total)
print *, total
if (abs(total - 6d0) > 1d-12) error stop 4
end program
