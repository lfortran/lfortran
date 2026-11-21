! External procedures with an assumed-shape dummy and saved state, whose
! dummies' bounds and lengths inquire about local objects (F2018 10.1.11),
! called directly, through a dummy procedure, through a procedure pointer
! and with a section: all calls share one saved variable.
!
! In save_41_t and save_41_g the bound or length of the local, or of the
! dummy itself, is a reference to a type-bound procedure or to a generic
! interface, which is resolved only after the other declarations.
!
! In save_41_r the extents and lengths are known only at run time, can be
! zero or negative, and the DIM of an inquiry is not constant.
!
! In save_41_c and save_41_b each local's bounds inquire about the previous
! local, which must not make the compile time grow exponentially.
!
! gfortran evaluates the bounds and lengths of a dummy before it allocates
! the automatic objects that they inquire about, so it gives such dummies
! wrong bounds and lengths. Their values are therefore checked only with
! other compilers (`strict`), while the saved counter is checked with every
! compiler.
module save_41_m
implicit none
type tt
    integer :: n
contains
    procedure :: f
end type
contains
    pure integer function f(self, m)
    class(tt), intent(in) :: self
    integer, intent(in) :: m
    f = self%n + m
    end function
end module

pure integer function save_41_g1(k)
integer, intent(in) :: k
save_41_g1 = k + 1
end function

subroutine save_41_t(a, t, x, y, s, r)
use save_41_m, only: tt
double precision, intent(in) :: a(:)
type(tt), intent(in) :: t
integer :: w(t%f(1))
integer :: v(t%n + 1)
character(len=t%f(2)) :: c
character(len=t%n) :: e
double precision, intent(inout) :: x(ubound(w, 1))
double precision, intent(inout) :: y(t%f(size(v)))
character(len=len(c)), intent(in) :: s
character(len=t%f(len(e))), intent(in) :: r
integer, save :: k = 0
k = k + 1
x(1) = k
y(1) = 10 * k + size(a)
y(2) = 100 * len(s) + len(r)
end subroutine

subroutine save_41_g(a, n, x, y, s)
double precision, intent(in) :: a(:)
integer, intent(in) :: n
interface gen
    pure integer function save_41_g1(k)
    integer, intent(in) :: k
    end function
end interface
integer :: w(gen(n))
integer :: v(n + 1)
character(len=gen(n)) :: c
double precision, intent(inout) :: x(ubound(w, 1))
double precision, intent(inout) :: y(gen(size(v)))
character(len=len(c)), intent(in) :: s
integer, save :: k = 0
k = k + 1
x(1) = k
y(1) = 10 * k + size(a)
y(2) = len(s)
end subroutine

subroutine save_41_r(a, n, m, d, x, y, z, s, info)
double precision, intent(in) :: a(:)
integer, intent(in) :: n, m, d
integer :: w(n:m, 3:m)
integer :: u(lbound(w, 1):ubound(w, 1) - lbound(w, 1))
character(len=m) :: c
double precision, intent(inout) :: x(lbound(w, 1):ubound(w, 1) + 1)
double precision, intent(inout) :: y(size(w, d) + 1)
double precision, intent(inout) :: z(lbound(u, 1):ubound(u, 1) + size(w))
character(len=len(c) + 1), intent(in) :: s
integer, intent(out) :: info(8)
integer, save :: k = 0
k = k + 1
info = [k, lbound(x, 1), ubound(x, 1), size(y), lbound(z, 1), ubound(z, 1), &
    len(s), size(a)]
end subroutine

subroutine save_41_c(a, n, x, info)
double precision, intent(in) :: a(:)
integer, intent(in) :: n
integer :: v1(n)
integer :: v2(ubound(v1, 1) + 1)
integer :: v3(ubound(v2, 1) + 1)
integer :: v4(ubound(v3, 1) + 1)
integer :: v5(ubound(v4, 1) + 1)
integer :: v6(ubound(v5, 1) + 1)
integer :: v7(ubound(v6, 1) + 1)
integer :: v8(ubound(v7, 1) + 1)
integer :: v9(ubound(v8, 1) + 1)
integer :: v10(ubound(v9, 1) + 1)
integer :: v11(ubound(v10, 1) + 1)
integer :: v12(ubound(v11, 1) + 1)
double precision, intent(inout) :: x(ubound(v12, 1))
integer, intent(out) :: info(3)
integer, save :: k = 0
k = k + 1
info = [k, size(x), size(a)]
end subroutine

subroutine save_41_b(n, x, info)
integer, intent(in) :: n
integer :: w1(-n:n)
integer :: w2(lbound(w1, 1):ubound(w1, 1) - lbound(w1, 1))
integer :: w3(lbound(w2, 1):ubound(w2, 1) - lbound(w2, 1))
integer :: w4(lbound(w3, 1):ubound(w3, 1) - lbound(w3, 1))
integer :: w5(lbound(w4, 1):ubound(w4, 1) - lbound(w4, 1))
integer :: w6(lbound(w5, 1):ubound(w5, 1) - lbound(w5, 1))
integer :: w7(lbound(w6, 1):ubound(w6, 1) - lbound(w6, 1))
integer :: w8(lbound(w7, 1):ubound(w7, 1) - lbound(w7, 1))
integer :: w9(lbound(w8, 1):ubound(w8, 1) - lbound(w8, 1))
integer :: w10(lbound(w9, 1):ubound(w9, 1) - lbound(w9, 1))
integer :: w11(lbound(w10, 1):ubound(w10, 1) - lbound(w10, 1))
integer :: w12(lbound(w11, 1):ubound(w11, 1) - lbound(w11, 1))
double precision, intent(inout) :: x(lbound(w12, 1):ubound(w12, 1))
integer, intent(out) :: info(2)
info = [lbound(x, 1), ubound(x, 1)]
end subroutine

program save_41
use iso_fortran_env, only: compiler_version
use save_41_m, only: tt
implicit none
interface
    subroutine save_41_t(a, t, x, y, s, r)
    import :: tt
    double precision, intent(in) :: a(:)
    type(tt), intent(in) :: t
    double precision, intent(inout) :: x(*)
    double precision, intent(inout) :: y(*)
    character(len=*), intent(in) :: s
    character(len=*), intent(in) :: r
    end subroutine
    subroutine save_41_g(a, n, x, y, s)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: n
    double precision, intent(inout) :: x(*)
    double precision, intent(inout) :: y(*)
    character(len=*), intent(in) :: s
    end subroutine
    subroutine save_41_r(a, n, m, d, x, y, z, s, info)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: n, m, d
    double precision, intent(inout) :: x(*)
    double precision, intent(inout) :: y(*)
    double precision, intent(inout) :: z(*)
    character(len=*), intent(in) :: s
    integer, intent(out) :: info(8)
    end subroutine
    subroutine save_41_c(a, n, x, info)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: n
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(3)
    end subroutine
    subroutine save_41_b(n, x, info)
    integer, intent(in) :: n
    double precision, intent(inout) :: x(*)
    integer, intent(out) :: info(2)
    end subroutine
end interface
procedure(save_41_t), pointer :: p
procedure(save_41_g), pointer :: q
procedure(save_41_r), pointer :: pr
procedure(save_41_c), pointer :: pc
double precision :: a(3), x(40), y(40), z(40)
character(len=10) :: str
type(tt) :: t
integer :: info(8)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
a = 1
t%n = 2
str = "abcdefghij"

call save_41_t(a, t, x, y, str, str)
if (x(1) /= 1 .or. y(1) /= 13) error stop 1
if (strict .and. y(2) /= 404) error stop 1
call run_t(save_41_t)
if (x(1) /= 2 .or. y(1) /= 23) error stop 2
if (strict .and. y(2) /= 404) error stop 2
p => save_41_t
call p(a, t, x, y, str, str)
if (x(1) /= 3 .or. y(1) /= 33) error stop 3
if (strict .and. y(2) /= 404) error stop 3
call save_41_t(a(1:3:2), t, x, y, str, str)
if (x(1) /= 4 .or. y(1) /= 42) error stop 4
if (strict .and. y(2) /= 404) error stop 4
call save_41_t(a, t, x, y, str, str)
if (x(1) /= 5 .or. y(1) /= 53) error stop 5
if (strict .and. y(2) /= 404) error stop 5

call save_41_g(a, 2, x, y, str)
if (x(1) /= 1 .or. y(1) /= 13) error stop 6
if (strict .and. y(2) /= 3) error stop 6
call run_g(save_41_g)
if (x(1) /= 2 .or. y(1) /= 23) error stop 7
if (strict .and. y(2) /= 3) error stop 7
q => save_41_g
call q(a, 2, x, y, str)
if (x(1) /= 3 .or. y(1) /= 33) error stop 8
if (strict .and. y(2) /= 3) error stop 8
call save_41_g(a(1:3:2), 2, x, y, str)
if (x(1) /= 4 .or. y(1) /= 42) error stop 9
if (strict .and. y(2) /= 3) error stop 9
call save_41_g(a, 2, x, y, str)
if (x(1) /= 5 .or. y(1) /= 53) error stop 10
if (strict .and. y(2) /= 3) error stop 10

! Non-zero extents, DIM 1.
call save_41_r(a, -2, 5, 1, x, y, z, str, info)
call check(info, [1, -2, 6, 9, -2, 31, 6, 3], 11)
! Non-zero extents, DIM 2.
call run_r(save_41_r, -2, 5, 2)
call check(info, [2, -2, 6, 4, -2, 31, 6, 3], 12)
! A zero extent in the first dimension, and so in a dimension of `u`.
pr => save_41_r
call pr(a, 5, 3, 1, x, y, z, str, info)
call check(info, [3, 1, 1, 1, 1, 0, 4, 3], 13)
! A zero extent in the second dimension, selected by DIM.
call save_41_r(a(1:3:2), 0, 1, 2, x, y, z, str, info)
call check(info, [4, 0, 2, 1, 0, 1, 2, 2], 14)
! A zero-length local and a zero extent in the second dimension.
call save_41_r(a, 0, 0, 1, x, y, z, str, info)
call check(info, [5, 0, 1, 2, 0, 0, 1, 3], 15)

call save_41_c(a, 3, x, info(1:3))
call check_c(1, 3, 16)
call run_c(save_41_c)
call check_c(2, 3, 17)
pc => save_41_c
call pc(a, 3, x, info(1:3))
call check_c(3, 3, 18)
call save_41_c(a(1:3:2), 3, x, info(1:3))
call check_c(4, 2, 19)

call save_41_b(3, x, info(1:2))
if (strict .and. any(info(1:2) /= [-3, 36])) error stop 20
print *, x(1), y(1), info
contains
    subroutine run_t(f)
    procedure(save_41_t) :: f
    call f(a, t, x, y, str, str)
    end subroutine
    subroutine run_g(f)
    procedure(save_41_g) :: f
    call f(a, 2, x, y, str)
    end subroutine
    subroutine run_r(f, n, m, d)
    procedure(save_41_r) :: f
    integer, intent(in) :: n, m, d
    call f(a, n, m, d, x, y, z, str, info)
    end subroutine
    subroutine run_c(f)
    procedure(save_41_c) :: f
    call f(a, 3, x, info(1:3))
    end subroutine
    subroutine check_c(k, size_a, code)
    integer, intent(in) :: k, size_a, code
    if (info(1) /= k .or. info(3) /= size_a) error stop code
    if (strict .and. info(2) /= 14) error stop code
    end subroutine
    subroutine check(info, expected, code)
    integer, intent(in) :: info(8), expected(8), code
    if (info(1) /= expected(1) .or. info(8) /= expected(8)) error stop code
    if (strict .and. any(info /= expected)) error stop code
    end subroutine
end program
