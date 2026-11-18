! A procedure with an assumed-shape dummy and a saved local: COMMON
! variables in the bounds or length of its dummies, and intent(out) dummies
! that are finalized, default-initialized or deallocated once per call,
! also in a separate module procedure.
module save_33_mod
implicit none
integer :: nfinal = 0, nfinal_scalar = 0
type :: fin_t
    integer :: k = 7
    integer, allocatable :: a(:)
contains
    final :: fin_array
end type
type :: fin_scalar_t
    integer :: k = 3
contains
    final :: fin_scalar
end type
contains

subroutine fin_array(self)
type(fin_t), intent(inout) :: self(:)
nfinal = nfinal + size(self)
end subroutine

subroutine fin_scalar(self)
type(fin_scalar_t), intent(inout) :: self
nfinal_scalar = nfinal_scalar + 1
end subroutine

subroutine reset(x, d, s, al, n)
real, intent(in) :: x(:)
type(fin_t), intent(out) :: d(:)
type(fin_scalar_t), intent(out) :: s
integer, allocatable, intent(out) :: al(:)
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
if (any(d%k /= 7)) error stop
if (s%k /= 3) error stop
if (allocated(al)) error stop
if (allocated(d(1)%a)) error stop
d(1)%k = d(1)%k + c
s%k = s%k + c
allocate(al(size(x)), d(1)%a(2))
al = c
n = c
end subroutine

end module

module save_33_sep
use save_33_mod, only: fin_t
implicit none
interface
    module subroutine reset_sep(x, d, n)
    real, intent(in) :: x(:)
    type(fin_t), intent(out) :: d(:)
    integer, intent(out) :: n
    end subroutine
end interface
end module

submodule (save_33_sep) save_33_sep_impl
contains
module subroutine reset_sep(x, d, n)
real, intent(in) :: x(:)
type(fin_t), intent(out) :: d(:)
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
if (any(d%k /= 7)) error stop
d%k = c + size(x)
n = c
end subroutine
end submodule

subroutine save_33_bounds(x, y, n)
integer :: nn
common /save_33_blk/ nn
real, intent(in) :: x(:)
real, intent(inout) :: y(nn)
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
y = y + sum(x)
n = c
end subroutine

subroutine save_33_length(x, s, n)
integer :: nn
common /save_33_blk/ nn
real, intent(in) :: x(:)
character(len=nn), intent(inout) :: s
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
s(1:1) = achar(iachar('0') + c)
n = c
end subroutine

program save_33
use save_33_mod, only: fin_t, fin_scalar_t, reset, nfinal, nfinal_scalar
use save_33_sep, only: reset_sep
implicit none
interface
subroutine save_33_bounds(x, y, n)
integer :: nn
common /save_33_blk/ nn
real, intent(in) :: x(:)
real, intent(inout) :: y(nn)
integer, intent(out) :: n
end subroutine
subroutine save_33_length(x, s, n)
integer :: nn
common /save_33_blk/ nn
real, intent(in) :: x(:)
character(len=nn), intent(inout) :: s
integer, intent(out) :: n
end subroutine
end interface
integer :: nn
common /save_33_blk/ nn
real :: a(4), b(3, 4), y(3)
character(len=3) :: s
type(fin_t) :: d(4), e(3, 4)
type(fin_scalar_t) :: t
integer, allocatable :: al(:)
integer :: n, f0, g0

nn = 3
a = 1.0
b = 2.0
y = 0.0
call save_33_bounds(a, y, n)
if (n /= 1) error stop
call save_33_bounds(a, y, n)
if (n /= 2) error stop
call save_33_bounds(b(1, :), y, n)
if (n /= 3) error stop
if (any(abs(y - 16.0) > 1e-6)) error stop

s = 'xyz'
call save_33_length(a, s, n)
if (n /= 1 .or. s /= '1yz') error stop
call save_33_length(b(2, :), s, n)
if (n /= 2 .or. s /= '2yz') error stop
call save_33_length(a, s, n)
if (n /= 3 .or. s /= '3yz') error stop

d%k = 1
e%k = 1
t%k = 1
f0 = nfinal
g0 = nfinal_scalar
call reset(a, d, t, al, n)
if (n /= 1 .or. nfinal - f0 /= 4 .or. nfinal_scalar - g0 /= 1) error stop
if (d(1)%k /= 8 .or. t%k /= 4 .or. size(al) /= 4) error stop
f0 = nfinal
g0 = nfinal_scalar
call reset(b(1, :), e(1, :), t, al, n)
if (n /= 2 .or. nfinal - f0 /= 4 .or. nfinal_scalar - g0 /= 1) error stop
if (e(1, 1)%k /= 9 .or. t%k /= 5 .or. any(al /= 2)) error stop
f0 = nfinal
g0 = nfinal_scalar
call reset(a, d, t, al, n)
if (n /= 3 .or. nfinal - f0 /= 4 .or. nfinal_scalar - g0 /= 1) error stop
if (d(1)%k /= 10 .or. t%k /= 6 .or. any(al /= 3)) error stop

f0 = nfinal
call reset_sep(a, d, n)
if (n /= 1 .or. nfinal - f0 /= 4 .or. any(d%k /= 5)) error stop
f0 = nfinal
call reset_sep(b(1, :), e(1, :), n)
if (n /= 2 .or. nfinal - f0 /= 4 .or. any(e(1, :)%k /= 6)) error stop

print *, "ok"
end program
