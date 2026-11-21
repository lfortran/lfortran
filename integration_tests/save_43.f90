! External procedures with an assumed-shape dummy and saved state, whose
! dummy's bounds inquire about automatic objects of a finalizable derived
! type (save_42_d) or of a type with a finalizable component (save_42_c),
! or about a component of such objects (save_42_m), whose bounds inquire
! about each other with a DIM known only at run time.
! Calls made directly, through a dummy procedure, through a procedure
! pointer and with a section share one saved variable, and finalize each
! automatic object once per call.
!
! gfortran evaluates the bounds of a dummy before it allocates the automatic
! objects that they inquire about, so it gives such dummies wrong bounds.
! They are therefore checked only with other compilers (`strict`), while
! the saved counter and the finalizations are checked with every compiler.
module save_42_types
implicit none
type :: t
    integer :: c = 0
contains
    final :: fin
end type
type :: outer
    type(t) :: c
end type
integer :: nfin = 0
contains
    impure elemental subroutine fin(y)
    type(t), intent(inout) :: y
    nfin = nfin + 1
    end subroutine
end module

subroutine save_42_d(a, n, lo, dd, x, info)
use save_42_types, only: t
integer, intent(in) :: n, lo, dd
double precision, intent(in) :: a(:)
type(t) :: w1(lo:n, lo:n+1, lo:n+2)
type(t) :: w2(lbound(w1, dd):ubound(w1, dd), lo:ubound(w1, 2), size(w1, dd))
type(t) :: w3(lbound(w2, dd):ubound(w2, dd), lo:ubound(w2, 2), size(w2, dd))
double precision, intent(inout) :: x(lbound(w3, dd):ubound(w3, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_42_c(a, n, lo, dd, x, info)
use save_42_types, only: outer
integer, intent(in) :: n, lo, dd
double precision, intent(in) :: a(:)
integer :: w1(lo:n, lo:n+1, lo:n+2)
integer :: w2(lbound(w1, dd):ubound(w1, dd), lo:ubound(w1, 2), size(w1, dd))
type(outer) :: w3(lbound(w2, dd):ubound(w2, dd), lo:ubound(w2, 2), size(w2, dd))
double precision, intent(inout) :: x(lbound(w3, dd):ubound(w3, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_42_m(a, n, lo, dd, x, info)
use save_42_types, only: t
integer, intent(in) :: n, lo, dd
double precision, intent(in) :: a(:)
type(t) :: w1(lo:n, lo:n+1, lo:n+2)
type(t) :: w2(lbound(w1, dd):ubound(w1, dd), lo:ubound(w1, 2), size(w1, dd))
type(t) :: w3(lbound(w2, dd):ubound(w2, dd), lo:ubound(w2, 2), size(w2, dd))
double precision, intent(inout) :: x(size(w3%c, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

program save_43
use iso_fortran_env, only: compiler_version
use save_42_types, only: nfin
implicit none
interface
    subroutine save_42_d(a, n, lo, dd, x, info)
    integer, intent(in) :: n, lo, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_42_c(a, n, lo, dd, x, info)
    integer, intent(in) :: n, lo, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_42_m(a, n, lo, dd, x, info)
    integer, intent(in) :: n, lo, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
end interface
procedure(save_42_d), pointer :: pd
procedure(save_42_c), pointer :: pc
procedure(save_42_m), pointer :: pm
double precision :: a(3), x(40)
integer :: info(4)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
a = 1

! With n = 3 and lo = -1, w1 is w1(-1:3, -1:4, -1:5) (210 elements).
! x(lbound(w3, dd):ubound(w3, dd)) is x(-1:3) for DIM 1, x(-1:4) for DIM 2
! and x(1:7) for DIM 3, and w2 and w3 have 150, 216 and 294 elements each.
call save_42_d(a, 3, -1, 1, x, info)
call check(info, [1, -1, 3, 3], 510, 1)
call run_d(save_42_d, 2)
call check(info, [2, -1, 4, 3], 642, 2)
pd => save_42_d
call pd(a, 3, -1, 3, x, info)
call check(info, [3, 1, 7, 3], 798, 3)
call save_42_d(a(1:3:2), 3, -1, 1, x, info)
call check(info, [4, -1, 3, 2], 510, 4)
call save_42_d(a, 3, -1, 3, x, info)
call check(info, [5, 1, 7, 3], 798, 5)

! Only w3 is finalizable, through its component.
call save_42_c(a, 3, -1, 1, x, info)
call check(info, [1, -1, 3, 3], 150, 6)
call run_c(save_42_c, 2)
call check(info, [2, -1, 4, 3], 216, 7)
pc => save_42_c
call pc(a, 3, -1, 3, x, info)
call check(info, [3, 1, 7, 3], 294, 8)
call save_42_c(a(1:3:2), 3, -1, 1, x, info)
call check(info, [4, -1, 3, 2], 150, 9)
call save_42_c(a, 3, -1, 3, x, info)
call check(info, [5, 1, 7, 3], 294, 10)

! x(size(w3%c, dd)) is x(1:5), x(1:6) and x(1:7) for DIM 1, 2 and 3.
call save_42_m(a, 3, -1, 1, x, info)
call check(info, [1, 1, 5, 3], 510, 11)
call run_m(save_42_m, 2)
call check(info, [2, 1, 6, 3], 642, 12)
pm => save_42_m
call pm(a, 3, -1, 3, x, info)
call check(info, [3, 1, 7, 3], 798, 13)
call save_42_m(a(1:3:2), 3, -1, 1, x, info)
call check(info, [4, 1, 5, 2], 510, 14)
call save_42_m(a, 3, -1, 3, x, info)
call check(info, [5, 1, 7, 3], 798, 15)
print *, info, nfin
contains
    subroutine run_d(f, dd)
    procedure(save_42_d) :: f
    integer, intent(in) :: dd
    call f(a, 3, -1, dd, x, info)
    end subroutine
    subroutine run_c(f, dd)
    procedure(save_42_c) :: f
    integer, intent(in) :: dd
    call f(a, 3, -1, dd, x, info)
    end subroutine
    subroutine run_m(f, dd)
    procedure(save_42_m) :: f
    integer, intent(in) :: dd
    call f(a, 3, -1, dd, x, info)
    end subroutine
    subroutine check(info, expected, finalized, code)
    integer, intent(in) :: info(4), expected(4), finalized, code
    if (info(1) /= expected(1) .or. info(4) /= expected(4)) error stop code
    if (strict .and. any(info /= expected)) error stop code
    if (nfin /= finalized) error stop 100 + code
    nfin = 0
    end subroutine
end program
