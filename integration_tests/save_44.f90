! External procedures with an assumed-shape dummy and saved state, whose
! dummies' bounds inquire with a DIM known only at run time about an
! element or a component of a local object of a finalizable type: an array
! component of an element (save_44_e), of a scalar (save_44_c, called only
! directly, and the function save_44_f), of a component of an element and
! a scalar component of a component of an array (save_44_n), and both a
! local and a component of its element in a chain of locals whose bounds
! inquire about each other (save_44_r).
! Calls made directly, through a dummy procedure, through a procedure
! pointer and with a section share one saved variable, and finalize each
! local object once per call.
!
! gfortran evaluates the bounds of a dummy before it allocates the automatic
! objects that they inquire about, so it gives such dummies wrong bounds.
! They are therefore checked only with other compilers (`strict`), while
! the saved counter and the finalizations are checked with every compiler.
module save_44_types
implicit none
type :: t
    integer :: i = 0
    integer :: v(3, 4)
    integer :: u(2:6)
contains
    final :: fin
end type
type :: outer
    type(t) :: c(2)
    type(t) :: d
end type
integer :: nfin = 0
contains
    impure elemental subroutine fin(y)
    type(t), intent(inout) :: y
    nfin = nfin + 1
    end subroutine
end module

subroutine save_44_e(a, n, dd, x, info)
use save_44_types, only: t
integer, intent(in) :: n, dd
double precision, intent(in) :: a(:)
type(t) :: w(n + 1)
double precision, intent(inout) :: x(size(w(1)%v, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_44_c(a, dd, x, info)
use save_44_types, only: t
integer, intent(in) :: dd
double precision, intent(in) :: a(:)
type(t) :: w
double precision, intent(inout) :: x(lbound(w%u, dd):ubound(w%u, dd) + size(w%v, dd + 1))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

integer function save_44_f(a, dd, x, info) result(r)
use save_44_types, only: t
integer, intent(in) :: dd
double precision, intent(in) :: a(:)
type(t) :: w
double precision, intent(inout) :: x(size(w%v, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
r = k
end function

subroutine save_44_n(a, n, dd, one, x, info)
use save_44_types, only: outer
integer, intent(in) :: n, dd, one
double precision, intent(in) :: a(:)
type(outer) :: w(n)
double precision, intent(inout) :: x(size(w(n)%c(1)%v, dd) + size(w%d%i, one))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

subroutine save_44_r(a, n, dd, x, info)
use save_44_types, only: t
integer, intent(in) :: n, dd
double precision, intent(in) :: a(:)
type(t) :: w1(-1:n, -1:n+1, -1:n+2)
type(t) :: w2(lbound(w1, dd):ubound(w1, dd), -1:ubound(w1, 2), size(w1, dd))
type(t) :: w3(lbound(w2, dd):ubound(w2, dd), -1:ubound(w2, 2), size(w2, dd))
double precision, intent(inout) :: x(size(w3, dd) + size(w3(1, 1, 1)%v, dd))
integer, intent(out) :: info(4)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(a)]
end subroutine

program save_44
use iso_fortran_env, only: compiler_version
use save_44_types, only: nfin
implicit none
interface
    subroutine save_44_e(a, n, dd, x, info)
    integer, intent(in) :: n, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_44_c(a, dd, x, info)
    integer, intent(in) :: dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    integer function save_44_f(a, dd, x, info) result(r)
    integer, intent(in) :: dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end function
    subroutine save_44_n(a, n, dd, one, x, info)
    integer, intent(in) :: n, dd, one
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
    subroutine save_44_r(a, n, dd, x, info)
    integer, intent(in) :: n, dd
    double precision, intent(in) :: a(:)
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(4)
    end subroutine
end interface
procedure(save_44_e), pointer :: pe
procedure(save_44_f), pointer :: pf
procedure(save_44_n), pointer :: pn
procedure(save_44_r), pointer :: pr
double precision :: a(3), x(20)
integer :: info(4)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
a = 1

! w(4): x(size(w(1)%v, dd)) is x(1:3) for DIM 1 and x(1:4) for DIM 2.
call save_44_e(a, 3, 1, x, info)
call check(info, [1, 1, 3, 3], 4, 1)
call run_e(save_44_e, 2)
call check(info, [2, 1, 4, 3], 4, 2)
pe => save_44_e
call pe(a, 3, 1, x, info)
call check(info, [3, 1, 3, 3], 4, 3)
call save_44_e(a(1:3:2), 3, 2, x, info)
call check(info, [4, 1, 4, 2], 4, 4)
call save_44_e(a, 3, 2, x, info)
call check(info, [5, 1, 4, 3], 4, 5)

! x(lbound(w%u, 1):ubound(w%u, 1) + size(w%v, 2)) is x(2:10).
call save_44_c(a, 1, x, info)
call check(info, [1, 2, 10, 3], 1, 6)
call save_44_c(a(2:3), 1, x, info)
call check(info, [2, 2, 10, 2], 1, 7)

! x(size(w%v, dd)) is x(1:3) for DIM 1 and x(1:4) for DIM 2.
if (save_44_f(a, 1, x, info) /= 1) error stop 8
call check(info, [1, 1, 3, 3], 1, 8)
if (run_f(save_44_f, 2) /= 2) error stop 9
call check(info, [2, 1, 4, 3], 1, 9)
pf => save_44_f
if (pf(a, 2, x, info) /= 3) error stop 10
call check(info, [3, 1, 4, 3], 1, 10)
if (save_44_f(a(1:3:2), 1, x, info) /= 4) error stop 11
call check(info, [4, 1, 3, 2], 1, 11)

! w(3) of a type with three finalizable components:
! x(size(w(3)%c(1)%v, dd) + size(w%d%i, 1)) is x(1:6) for DIM 1 and
! x(1:7) for DIM 2.
call save_44_n(a, 3, 1, 1, x, info)
call check(info, [1, 1, 6, 3], 9, 12)
call run_n(save_44_n, 2)
call check(info, [2, 1, 7, 3], 9, 13)
pn => save_44_n
call pn(a, 3, 1, 1, x, info)
call check(info, [3, 1, 6, 3], 9, 14)
call save_44_n(a(1:3:2), 3, 2, 1, x, info)
call check(info, [4, 1, 7, 2], 9, 15)

! With n = 3, w1 is w1(-1:3, -1:4, -1:5) (210 elements), w2 and w3 have
! 150 elements each for DIM 1 and 216 for DIM 2, and x(size(w3, dd) +
! size(w3(1, 1, 1)%v, dd)) is x(1:8) for DIM 1 and x(1:10) for DIM 2.
call save_44_r(a, 3, 1, x, info)
call check(info, [1, 1, 8, 3], 510, 16)
call run_r(save_44_r, 2)
call check(info, [2, 1, 10, 3], 642, 17)
pr => save_44_r
call pr(a, 3, 1, x, info)
call check(info, [3, 1, 8, 3], 510, 18)
call save_44_r(a(1:3:2), 3, 2, x, info)
call check(info, [4, 1, 10, 2], 642, 19)
call save_44_r(a, 3, 2, x, info)
call check(info, [5, 1, 10, 3], 642, 20)
print *, info, nfin
contains
    subroutine run_e(f, dd)
    procedure(save_44_e) :: f
    integer, intent(in) :: dd
    call f(a, 3, dd, x, info)
    end subroutine
    integer function run_f(f, dd)
    procedure(save_44_f) :: f
    integer, intent(in) :: dd
    run_f = f(a, dd, x, info)
    end function
    subroutine run_n(f, dd)
    procedure(save_44_n) :: f
    integer, intent(in) :: dd
    call f(a, 3, dd, 1, x, info)
    end subroutine
    subroutine run_r(f, dd)
    procedure(save_44_r) :: f
    integer, intent(in) :: dd
    call f(a, 3, dd, x, info)
    end subroutine
    subroutine check(info, expected, finalized, code)
    integer, intent(in) :: info(4), expected(4), finalized, code
    if (info(1) /= expected(1) .or. info(4) /= expected(4)) error stop code
    if (strict .and. any(info /= expected)) error stop code
    if (nfin /= finalized) error stop 100 + code
    nfin = 0
    end subroutine
end program
