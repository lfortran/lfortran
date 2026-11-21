! External procedures with an assumed-shape dummy and saved state, whose
! dummies' bounds inquire about local objects that are of derived type,
! saved, initialized by DATA or covered by a bare SAVE statement (F2018
! 10.1.11), called directly, through a dummy procedure, through a
! procedure pointer and with a section: all calls share one saved variable.
module save_40_m
implicit none
type t
    integer :: i
end type
contains
    ! The inquiries keep their values for a zero-extent dimension.
    subroutine save_40_z(d, z, y, info)
    integer, intent(in) :: d
    integer, save :: v(5:2, 2:4)
    double precision, intent(inout) :: z(lbound(v, d):ubound(v, d) + 1)
    double precision, intent(inout) :: y(size(v, d) + 1)
    integer, intent(out) :: info(3)
    info = [lbound(z, 1), ubound(z, 1), size(y)]
    end subroutine
end module

subroutine save_40_s(a, n, d, x, y, z)
use save_40_m, only: t
double precision, intent(in) :: a(:)
integer, intent(in) :: n, d
type(t) :: w(n+1)
integer :: v(3, 4)
character(len=5), save :: c(3, 4)
double precision, intent(inout) :: x(ubound(w, 1))
double precision, intent(inout) :: y(size(v, d))
double precision, intent(inout) :: z(size(c, d) + len(c))
integer, save :: k = 0
data v /12*0/
k = k + 1
x(1) = k
y(1) = 10 * k
z(1) = 100 * k + size(a)
end subroutine

subroutine save_40_t(a, n, x)
double precision, intent(in) :: a(:)
integer, intent(in) :: n
integer :: w(-1:1)
save
double precision, intent(inout) :: x(ubound(w, 1) + n)
integer :: k = 0
k = k + 1
x(1) = k + size(a)
end subroutine

program save_40
use save_40_m, only: save_40_z
implicit none
interface
    subroutine save_40_s(a, n, d, x, y, z)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: n, d
    double precision, intent(inout) :: x(n+1)
    double precision, intent(inout) :: y(3)
    double precision, intent(inout) :: z(8)
    end subroutine
    subroutine save_40_t(a, n, x)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: n
    double precision, intent(inout) :: x(n+1)
    end subroutine
end interface
procedure(save_40_s), pointer :: p
procedure(save_40_t), pointer :: q
double precision :: a(3), x(3), y(3), z(8), b(8)
integer :: info(3)
a = 1
call save_40_s(a, 2, 1, x, y, z)
if (x(1) /= 1 .or. y(1) /= 10 .or. z(1) /= 103) error stop 1
call run(save_40_s)
if (x(1) /= 2 .or. y(1) /= 20 .or. z(1) /= 203) error stop 2
p => save_40_s
call p(a, 2, 1, x, y, z)
if (x(1) /= 3 .or. y(1) /= 30 .or. z(1) /= 303) error stop 3
call save_40_s(a(1:3:2), 2, 1, x, y, z)
if (x(1) /= 4 .or. y(1) /= 40 .or. z(1) /= 402) error stop 4
call save_40_s(a, 2, 1, x, y, z)
if (x(1) /= 5 .or. y(1) /= 50 .or. z(1) /= 503) error stop 5

call save_40_t(a, 2, x)
if (x(1) /= 4) error stop 6
call run_t(save_40_t)
if (x(1) /= 5) error stop 7
q => save_40_t
call q(a, 2, x)
if (x(1) /= 6) error stop 8
call save_40_t(a(1:3:2), 2, x)
if (x(1) /= 6) error stop 9
call save_40_t(a, 2, x)
if (x(1) /= 8) error stop 10

call save_40_z(1, z, b, info)
if (any(info /= [1, 1, 1])) error stop 11
call save_40_z(2, z, b, info)
if (any(info /= [2, 5, 4])) error stop 12
print *, x(1), y(1), z(1)
contains
    subroutine run(f)
    procedure(save_40_s) :: f
    call f(a, 2, 1, x, y, z)
    end subroutine
    subroutine run_t(f)
    procedure(save_40_t) :: f
    call f(a, 2, x)
    end subroutine
end program
