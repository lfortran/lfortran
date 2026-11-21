! Procedures with an assumed-shape dummy and saved state, whose dummies'
! bounds inquire with a DIM known only at run time about a complex part of
! a component of a local object of a finalizable type: of a scalar
! (save_45_s, save_45_i) and of an element of an array (save_45_a), and,
! called only directly, of a scalar in a module procedure (save_45_m).
! Calls made directly, through a dummy procedure, through a procedure
! pointer and with a section share one saved variable, and finalize each
! local object once per call.
!
! gfortran evaluates the bounds of a dummy before it allocates the automatic
! objects that they inquire about, so it gives such dummies wrong bounds.
! They are therefore checked only with other compilers (`strict`), while
! the saved counter and the finalizations are checked with every compiler.
module save_45_types
implicit none
type :: t
    integer :: i = 0
    complex :: cz(3, 4)
    complex :: cy(0:2, -1:3)
contains
    final :: fin
end type
integer :: nfin = 0
contains
    impure elemental subroutine fin(y)
    type(t), intent(inout) :: y
    nfin = nfin + 1
    end subroutine
end module

module save_45_m
use save_45_types, only: t
implicit none
contains
    subroutine save_45_mod(a, dd, x, info)
    integer, intent(in) :: dd
    double precision, intent(in) :: a(:)
    type(t) :: w
    double precision, intent(inout) :: x(ubound(w%cz%re, dd))
    integer, intent(out) :: info(4)
    integer, save :: k = 0
    k = k + 1
    info = [k, lbound(x, 1), ubound(x, 1), size(a)]
    end subroutine
end module

subroutine save_45_s(a, dd, x, info)
use save_45_types, only: t
integer, intent(in) :: dd
double precision, intent(in) :: a(:)
type(t) :: w
double precision, intent(inout) :: x(ubound(w%cz%re, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_45_i(a, dd, x, info)
use save_45_types, only: t
integer, intent(in) :: dd
double precision, intent(in) :: a(:)
type(t) :: w
double precision, intent(inout) :: x(lbound(w%cy%im, dd):ubound(w%cy%im, dd) + size(w%cz%im, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_45_a(a, n, dd, x, info)
use save_45_types, only: t
integer, intent(in) :: n, dd
double precision, intent(in) :: a(:)
type(t) :: w(n)
double precision, intent(inout) :: x(ubound(w(2)%cz%re, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

program save_45
use iso_fortran_env, only: compiler_version
use save_45_types, only: nfin
use save_45_m, only: save_45_mod
implicit none
interface
    subroutine save_45_s(a, dd, x, info)
    integer, intent(in) :: dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_45_i(a, dd, x, info)
    integer, intent(in) :: dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_45_a(a, n, dd, x, info)
    integer, intent(in) :: n, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
end interface
procedure(save_45_s), pointer :: ps
procedure(save_45_i), pointer :: pi
procedure(save_45_a), pointer :: pa
double precision :: a(3), x(20)
integer :: info(4)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
a = 1

! x(ubound(w%cz%re, dd)) is x(1:3) for DIM 1 and x(1:4) for DIM 2.
call save_45_s(a, 1, x, info)
call check(info, [1, 1, 3, 3], 1, 1)
call run_s(save_45_s, 2)
call check(info, [2, 1, 4, 3], 1, 2)
ps => save_45_s
call ps(a, 1, x, info)
call check(info, [3, 1, 3, 3], 1, 3)
call save_45_s(a(1:3:2), 2, x, info)
call check(info, [4, 1, 4, 2], 1, 4)

! A complex part is not a whole array, so its lower bounds are 1:
! x(lbound(w%cy%im, dd):ubound(w%cy%im, dd) + size(w%cz%im, dd)) is
! x(1:6) for DIM 1 and x(1:9) for DIM 2.
call save_45_i(a, 1, x, info)
call check(info, [1, 1, 6, 3], 1, 5)
call run_i(save_45_i, 2)
call check(info, [2, 1, 9, 3], 1, 6)
pi => save_45_i
call pi(a, 2, x, info)
call check(info, [3, 1, 9, 3], 1, 7)
call save_45_i(a(2:3), 1, x, info)
call check(info, [4, 1, 6, 2], 1, 8)

! w(5): x(ubound(w(2)%cz%re, dd)) is x(1:3) for DIM 1 and x(1:4) for DIM 2.
call save_45_a(a, 5, 1, x, info)
call check(info, [1, 1, 3, 3], 5, 9)
call run_a(save_45_a, 2)
call check(info, [2, 1, 4, 3], 5, 10)
pa => save_45_a
call pa(a, 5, 1, x, info)
call check(info, [3, 1, 3, 3], 5, 11)
call save_45_a(a(1:3:2), 5, 2, x, info)
call check(info, [4, 1, 4, 2], 5, 12)

call save_45_mod(a, 1, x, info)
call check(info, [1, 1, 3, 3], 1, 13)
call save_45_mod(a, 2, x, info)
call check(info, [2, 1, 4, 3], 1, 14)
call save_45_mod(a(1:2), 2, x, info)
call check(info, [3, 1, 4, 2], 1, 15)
print *, info, nfin
contains
    subroutine run_s(f, dd)
    procedure(save_45_s) :: f
    integer, intent(in) :: dd
    call f(a, dd, x, info)
    end subroutine
    subroutine run_i(f, dd)
    procedure(save_45_i) :: f
    integer, intent(in) :: dd
    call f(a, dd, x, info)
    end subroutine
    subroutine run_a(f, dd)
    procedure(save_45_a) :: f
    integer, intent(in) :: dd
    call f(a, 5, dd, x, info)
    end subroutine
    subroutine check(info, expected, finalized, code)
    integer, intent(in) :: info(4), expected(4), finalized, code
    if (info(1) /= expected(1) .or. info(4) /= expected(4)) error stop code
    if (strict .and. any(info /= expected)) error stop code
    if (nfin /= finalized) error stop 100 + code
    nfin = 0
    end subroutine
end program
