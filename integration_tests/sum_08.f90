! Reductions accumulated in a loop over allocatable and pointer arrays,
! operands and masks
module sum_08_mod
implicit none

type :: grid
    real(8), pointer :: x(:) => null()
    integer, allocatable :: k(:)
end type

contains

    real(8) function max_error(g, y) result(e)
        type(grid), intent(in) :: g
        real(8), intent(in) :: y(:)
        e = maxval(abs(g%x - y))
    end function

end module

program sum_08
use sum_08_mod
implicit none
real, allocatable :: x(:), c(:)
integer, allocatable :: k(:)
real, target :: t(4) = [1., 2., 3., 4.]
integer, target :: it(4) = [1, 2, 3, 4]
complex, target :: zt(2) = [(1., 1.), (2., -1.)]
real, pointer :: p(:) => null()
integer, pointer :: ip(:) => null()
complex, pointer :: zp(:) => null()
logical, allocatable :: mk(:)
logical, pointer :: lp(:) => null()
type(grid) :: g
real(8) :: y(3) = [1d0, 2d0, 3d0]
real :: s
integer :: i
complex :: z
logical :: l

allocate(x(4), c(4), k(4), mk(4), lp(4))
x = [1., -2., 3., 4.]
c = 2
k = [3, -1, 2, 5]
mk = [.true., .false., .true., .false.]
lp = [.false., .true., .true., .true.]
p => t; ip => it; zp => zt

! Allocatable array, the operation only in the mask
s = sum(x, mask = x > 0)
if (abs(s - 8) > 1e-6) error stop 1
i = sum(k, mask = k > 0)
if (i /= 10) error stop 2
s = maxval(x, mask = x < 0)
if (abs(s + 2) > 1e-6) error stop 3
s = minval(x, mask = x > 1)
if (abs(s - 3) > 1e-6) error stop 4
s = product(x, mask = x > 0)
if (abs(s - 12) > 1e-6) error stop 5
i = product(k, mask = k > 2)
if (i /= 15) error stop 6

! Allocatable operands
s = sum(x * c)
if (abs(s - 12) > 1e-6) error stop 7
s = maxval(x - 1)
if (abs(s - 3) > 1e-6) error stop 8
i = minval(k * 2)
if (i /= -2) error stop 9
l = any(x * c > 7)
if (.not. l) error stop 10
l = all(k + 2 > 0)
if (.not. l) error stop 11
i = count(x * c > 0)
if (i /= 3) error stop 12
s = dot_product(x * 2, c)
if (abs(s - 24) > 1e-6) error stop 13
s = sum(x * 2, mask = mk)
if (abs(s - 8) > 1e-6) error stop 14

! Pointer arrays and operands
s = sum(p, mask = p > 1)
if (abs(s - 9) > 1e-6) error stop 15
s = maxval(p + 1)
if (abs(s - 5) > 1e-6) error stop 16
s = minval(p * 2)
if (abs(s - 2) > 1e-6) error stop 17
s = product(p * 2)
if (abs(s - 384) > 1e-6) error stop 18
s = maxval(p * 2, mask = p < 3)
if (abs(s - 4) > 1e-6) error stop 19
i = maxval(ip * 2)
if (i /= 8) error stop 20
i = minval(ip + 1)
if (i /= 2) error stop 21
i = product(ip + 1)
if (i /= 120) error stop 22
i = sum(ip, mask = ip > 2)
if (i /= 7) error stop 23
l = any(p > 3.5)
if (.not. l) error stop 24
l = all(p * 2 > 1)
if (.not. l) error stop 25
i = count(p > 1)
if (i /= 3) error stop 26
s = dot_product(p * 2, p)
if (abs(s - 60) > 1e-6) error stop 27
z = sum(zp * 2)
if (abs(z - (6., 0.)) > 1e-6) error stop 28
z = product(zp * 2)
if (abs(z - (12., 4.)) > 1e-5) error stop 29
s = sum(p * c, mask = lp)
if (abs(s - 18) > 1e-6) error stop 30
i = count(lp .and. p > 2)
if (i /= 2) error stop 31

! Pointer and allocatable components
allocate(g%x(3), g%k(3))
g%x = [1d0, 2.5d0, 3d0]
g%k = [4, 5, 6]
if (abs(max_error(g, y) - 0.5d0) > 1d-12) error stop 32
if (abs(sum(g%x * 2) - 13d0) > 1d-12) error stop 33
if (abs(minval(g%x, mask = g%x > 1.5d0) - 2.5d0) > 1d-12) error stop 34
i = maxval(g%k - 1)
if (i /= 5) error stop 35
i = sum(g%k, mask = g%k > 4)
if (i /= 11) error stop 36

print *, "ok"
end program
