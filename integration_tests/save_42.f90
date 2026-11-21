! External procedures with an assumed-shape dummy and saved state, whose
! dummy's bounds inquire about local objects whose bounds inquire about each
! other with a DIM that is known only at run time (save_42_d: rank 3, three
! locals; save_42_s: rank 7, two locals) or with lower bounds other than 1
! (save_42_b: five locals). Calls made directly, through a dummy procedure,
! through a procedure pointer and with a section share one saved variable.
!
! gfortran evaluates the bounds of a dummy before it allocates the automatic
! objects that they inquire about, so it gives such dummies wrong bounds.
! They are therefore checked only with other compilers (`strict`), while
! the saved counter is checked with every compiler.
subroutine save_42_d(a, n, lo, dd, x, info)
integer, intent(in) :: n, lo, dd
double precision, intent(in) :: a(:)
integer :: w1(lo:n, lo:n+1, lo:n+2)
integer :: w2(lbound(w1, dd):ubound(w1, dd), lo:ubound(w1, 2), size(w1, dd))
integer :: w3(lbound(w2, dd):ubound(w2, dd), lo:ubound(w2, 2), size(w2, dd))
double precision, intent(inout) :: x(lbound(w3, dd):ubound(w3, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_42_s(a, n, lo, dd, x, info)
integer, intent(in) :: n, lo, dd
double precision, intent(in) :: a(:)
integer :: w1(lo:n, lo:n+1, lo:n+2, lo:n+3, lo:n+4, lo:n+5, lo:n+6)
integer :: w2(lo:ubound(w1, dd), lo:ubound(w1, dd), lo:ubound(w1, dd), &
    lo:ubound(w1, dd), lo:ubound(w1, dd), lo:ubound(w1, dd), lo:ubound(w1, dd))
double precision, intent(inout) :: x(lbound(w2, dd):ubound(w2, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_42_b(a, n, lo, x, info)
integer, intent(in) :: n, lo
double precision, intent(in) :: a(:)
integer :: w1(lo:n)
integer :: w2(lbound(w1, 1):ubound(w1, 1) - lbound(w1, 1))
integer :: w3(lbound(w2, 1):ubound(w2, 1) - lbound(w2, 1))
integer :: w4(lbound(w3, 1):ubound(w3, 1) - lbound(w3, 1))
integer :: w5(lbound(w4, 1):ubound(w4, 1) - lbound(w4, 1))
double precision, intent(inout) :: x(lbound(w5, 1):ubound(w5, 1))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

program save_42
use iso_fortran_env, only: compiler_version
implicit none
interface
    subroutine save_42_d(a, n, lo, dd, x, info)
    integer, intent(in) :: n, lo, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_42_s(a, n, lo, dd, x, info)
    integer, intent(in) :: n, lo, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_42_b(a, n, lo, x, info)
    integer, intent(in) :: n, lo
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
end interface
procedure(save_42_d), pointer :: pd
procedure(save_42_s), pointer :: ps
procedure(save_42_b), pointer :: pb
double precision :: a(3), x(40)
integer :: info(4)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
a = 1

! x(lbound(w3, dd):ubound(w3, dd)) is x(-2:3) for DIM 1, x(-2:4) for DIM 2
! and x(1:8) for DIM 3.
call save_42_d(a, 3, -2, 1, x, info)
call check(info, [1, -2, 3, 3], 1)
call run_d(save_42_d, 2)
call check(info, [2, -2, 4, 3], 2)
pd => save_42_d
call pd(a, 3, -2, 3, x, info)
call check(info, [3, 1, 8, 3], 3)
call save_42_d(a(1:3:2), 3, -2, 1, x, info)
call check(info, [4, -2, 3, 2], 4)
call save_42_d(a, 3, -2, 3, x, info)
call check(info, [5, 1, 8, 3], 5)

! x(lbound(w2, dd):ubound(w2, dd)) is x(0:dd-1).
call save_42_s(a, 0, 0, 2, x, info)
call check(info, [1, 0, 1, 3], 6)
call run_s(save_42_s, 3)
call check(info, [2, 0, 2, 3], 7)
ps => save_42_s
call ps(a, 0, 0, 1, x, info)
call check(info, [3, 0, 0, 3], 8)
call save_42_s(a(1:3:2), 0, 0, 2, x, info)
call check(info, [4, 0, 1, 2], 9)
call save_42_s(a, 0, 0, 3, x, info)
call check(info, [5, 0, 2, 3], 10)

! x(lbound(w5, 1):ubound(w5, 1)) is x(-2:11) for n = 3 and lo = -2.
call save_42_b(a, 3, -2, x, info)
call check(info, [1, -2, 11, 3], 11)
call run_b(save_42_b)
call check(info, [2, -2, 11, 3], 12)
pb => save_42_b
call pb(a, 3, -2, x, info)
call check(info, [3, -2, 11, 3], 13)
call save_42_b(a(1:3:2), 3, -2, x, info)
call check(info, [4, -2, 11, 2], 14)
call save_42_b(a, 3, -2, x, info)
call check(info, [5, -2, 11, 3], 15)
print *, info
contains
    subroutine run_d(f, dd)
    procedure(save_42_d) :: f
    integer, intent(in) :: dd
    call f(a, 3, -2, dd, x, info)
    end subroutine
    subroutine run_s(f, dd)
    procedure(save_42_s) :: f
    integer, intent(in) :: dd
    call f(a, 0, 0, dd, x, info)
    end subroutine
    subroutine run_b(f)
    procedure(save_42_b) :: f
    call f(a, 3, -2, x, info)
    end subroutine
    subroutine check(info, expected, code)
    integer, intent(in) :: info(4), expected(4), code
    if (info(1) /= expected(1) .or. info(4) /= expected(4)) error stop code
    if (strict .and. any(info /= expected)) error stop code
    end subroutine
end program
