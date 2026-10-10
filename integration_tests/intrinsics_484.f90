module intrinsics_484_mod
implicit none
contains

subroutine check_dummy(a, lo)
integer, intent(in) :: lo
integer, intent(in) :: a(lo:lo + 3)
if (any(cshift(a, 1) /= [20, 30, 40, 10])) error stop 101
if (lbound(cshift(a, 1), 1) /= 1) error stop 102
if (any(eoshift(a, 1) /= [20, 30, 40, 0])) error stop 103
if (lbound(eoshift(a, 1), 1) /= 1) error stop 104
end subroutine

end module

program intrinsics_484
use intrinsics_484_mod, only: check_dummy
implicit none
integer :: a(0:3), v(-2:5), s(-3:-1)
integer :: m(0:1, -1:1), n(-1:1, 0:1), mm(0:2, 0:3)
integer :: r(2, 3), c(4)
logical :: msk(0:3), umsk(-1:2)
integer :: i, j

a = [10, 20, 30, 40]
v = [(i, i = 1, 8)]
s = [7, 8, 9]
msk = [.true., .false., .true., .true.]
umsk = [.true., .false., .true., .false.]
do j = -1, 1
    do i = 0, 1
        m(i, j) = i + 1 + 2*(j + 1)
    end do
end do
do j = 0, 1
    do i = -1, 1
        n(i, j) = i + 2 + 3*j
    end do
end do
do j = 0, 3
    do i = 0, 2
        mm(i, j) = i + 1 + 3*j
    end do
end do
mm(1, 1) = 100

! cshift / eoshift
c = cshift(a, 1)
if (any(c /= [20, 30, 40, 10])) error stop 1
if (lbound(cshift(a, 1), 1) /= 1) error stop 2
if (any(eoshift(a, 1) /= [20, 30, 40, 0])) error stop 3
if (lbound(eoshift(a, -1), 1) /= 1) error stop 4
if (any(cshift(n, 1, 2) /= reshape([4, 5, 6, 1, 2, 3], [3, 2]))) error stop 5
if (any(eoshift(m, 1, 0, 2) /= reshape([3, 4, 5, 6, 0, 0], [2, 3]))) error stop 6
if (any(lbound(cshift(n, 1, 2)) /= 1)) error stop 7

! transpose
r = transpose(n)
if (any(r /= reshape([1, 4, 2, 5, 3, 6], [2, 3]))) error stop 8
if (any(lbound(transpose(n)) /= 1)) error stop 9

! pack with vector=
if (any(pack(a, msk, v(-2:1)) /= [10, 30, 40, 4])) error stop 10
if (any(pack(a, msk, v) /= [10, 30, 40, 4, 5, 6, 7, 8])) error stop 11
if (lbound(pack(a, msk, v), 1) /= 1) error stop 12

! unpack
if (any(unpack(s(-3:-2), umsk, 0) /= [7, 0, 8, 0])) error stop 13
if (any(unpack(s(-3:-2), umsk, a) /= [7, 20, 8, 40])) error stop 14
if (lbound(unpack(s(-3:-2), umsk, 0), 1) /= 1) error stop 15

! spread
if (any(spread(s(-2), 1, 3) /= [8, 8, 8])) error stop 16
if (any(spread(s, 1, 2) /= reshape([7, 7, 8, 8, 9, 9], [2, 3]))) error stop 17
if (any(lbound(spread(s, 2, 2)) /= 1)) error stop 18

! matmul
if (any(matmul(m, n) /= reshape([22, 28, 49, 64], [2, 2]))) error stop 19
if (any(lbound(matmul(m, n)) /= 1)) error stop 20
if (any(matmul(m, s) /= [76, 100])) error stop 21
if (any(matmul(a(0:1), m) /= [50, 110, 170])) error stop 22
if (lbound(matmul(a(0:1), m), 1) /= 1) error stop 23

! maxloc / minloc / findloc with dim=
if (any(maxloc(mm, dim=1) /= [3, 2, 3, 3])) error stop 24
if (any(lbound(maxloc(mm, dim=1)) /= 1)) error stop 25
if (any(minloc(mm, dim=2) /= [1, 1, 1])) error stop 26
if (any(findloc(mm, 100, dim=1) /= [0, 2, 0, 0])) error stop 27
if (any(lbound(findloc(mm, 100, dim=2)) /= 1)) error stop 28

! explicit-shape dummy with a run-time lower bound
call check_dummy(a, -5)
call check_dummy(a, 0)

print *, "ok"
end program
