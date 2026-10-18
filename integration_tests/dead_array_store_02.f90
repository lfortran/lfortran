! The dead_array_store pass keeps a whole-array assignment whenever
! something may read what it assigned before it is assigned again.
module dead_array_store_02_m
implicit none
real :: g(3)
contains
real function first_of_g()
    first_of_g = g(1)
end function
real function first(a)
    real, intent(in) :: a(:)
    first = a(1)
end function
end module

program dead_array_store_02
use dead_array_store_02_m, only: g, first_of_g, first
implicit none
real :: x(3), y(3), s
real, target :: t(3)
real, pointer :: p(:)
integer :: i, k
logical :: flag

! A module variable a call may read.
g = 1
s = first_of_g()
g = 2
if (s /= 1) error stop

! An actual argument.
x = 5
s = first(x)
x = 0
if (s /= 5) error stop

! A procedure contained here reads it by host association.
x = 7
call read_x()
x = 0
if (s /= 7) error stop

! The second assignment may not run.
flag = .false.
x = 3
if (flag) x = 4
if (any(x /= 3)) error stop

! `exit` may leave the loop before the second assignment.
do k = 1, 2
    y = real(k)
    if (k == 2) exit
    y = 0
end do
if (any(y /= 2)) error stop

! `exit` of a block may skip the second assignment.
named: block
    x = 8
    if (s > 0) exit named
    x = 9
end block named
if (any(x /= 8)) error stop

! Only part of it is assigned again.
x = 4
x(1:2) = 0
if (x(3) /= 4) error stop

! A pointer reads it.
p => t
t = 6
s = p(2)
t = 0
if (s /= 6) error stop

! The declaration of a block local reads it.
call declaration_reads()

! A namelist output statement reads it without naming it.
call namelist_reads()

! The value assigned again reads it.
y = 3
y = y * 2
if (any(y /= 6)) error stop

! A loop in between reads it.
x = 1
s = 0
do i = 1, 3
    s = s + x(i)
end do
x = 0
if (s /= 3) error stop
print *, "ok"

contains

subroutine read_x()
    s = x(1)
end subroutine

subroutine namelist_reads()
    integer :: v(2), u
    namelist /group/ v
    v = 5
    open(newunit=u, status="scratch")
    write(u, nml=group)
    v = 0
    rewind(u)
    read(u, nml=group)
    close(u)
    if (any(v /= 5)) error stop
end subroutine

subroutine declaration_reads()
    real :: v(3)
    v = 2
    block
        real :: w(int(v(1)))
        w = 1
        v = 0
        if (size(w) /= 2) error stop
    end block
end subroutine

end program
