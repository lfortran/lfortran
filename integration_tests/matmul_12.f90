program matmul_12
! matmul(matrix, vector) and matmul(matrix, matrix): all types and kinds,
! strided sections, transpose, zero-size dimensions, assumed-shape,
! allocatable and pointer operands, and results that alias an operand
! (#16085). The real and complex data are small dyadic numbers, so every
! sum is exact whatever the order of the additions.
implicit none
integer :: i, j, n, m
integer(1) :: i1(3,4), v1(4), r1(3)
integer(2) :: i2(3,4), v2(4), r2(3)
integer(4) :: i4(3,4), v4(4), r4(3)
integer(8) :: i8(3,4), v8(4), r8(3)
real(4) :: a4(6,8), x4(8), y4(6), z4(3), q4(8,2), p4(6,2)
real(8) :: a8(6,8), x8(8), y8(6)
complex(4) :: c4(3,4), u4(4), s4(3)
complex(8) :: c8(3,4), u8(4), s8(3), d8(3,2)
logical :: la(3,4), lv(4), lr(3)
logical :: lb(4,2), lm(3,2)
real(8), allocatable :: wa(:,:), va(:), ra(:)
real(8), target :: wt(4,4), vt(4)
real(8), pointer :: wp(:,:), vp(:)
real(8) :: sq(4,4), sv(4), sref(4), smat(4,4)
real :: rnd(37,23), rv(23), rr(37), rref(37)
real, allocatable :: la4(:,:), lv4(:), lc4(:,:), lr4(:), lm4(:,:)

! integer kinds
do j = 1, 4
    do i = 1, 3
        i4(i,j) = i - 2*j + 3
    end do
end do
v4 = [2, -1, 3, 1]
i1 = int(i4, 1); i2 = int(i4, 2); i8 = int(i4, 8)
v1 = int(v4, 1); v2 = int(v4, 2); v8 = int(v4, 8)
r4 = matmul(i4, v4)
call check_i4(r4, i4, v4)
r1 = matmul(i1, v1)
if (any(int(r1, 4) /= r4)) error stop 1
r2 = matmul(i2, v2)
if (any(int(r2, 4) /= r4)) error stop 2
r8 = matmul(i8, v8)
if (any(int(r8, 4) /= r4)) error stop 3
if (any(matmul(i4, [0, 0, 0, 0]) /= 0)) error stop 4

! real kinds, strided sections, transpose
do j = 1, 8
    do i = 1, 6
        a4(i,j) = real(mod(7*i + 3*j, 11) - 5) / 4
    end do
    x4(j) = real(mod(5*j, 7) - 3) / 2
end do
a8 = a4; x8 = x4
y4 = matmul(a4, x4)
call check_r4(y4, a4, x4)
y8 = matmul(a8, x8)
if (any(real(y8, 4) /= y4)) error stop 5
z4 = matmul(a4(1:6:2, :), x4)
call check_r4(z4, a4(1:6:2, :), x4)
z4 = matmul(a4(2:6:2, 1:8:2), x4(2:8:2))
call check_r4(z4, a4(2:6:2, 1:8:2), x4(2:8:2))
y4(1:4) = matmul(a4(3:6, 2:3), x4(5:6))
call check_r4(y4(1:4), a4(3:6, 2:3), x4(5:6))
x4 = matmul(transpose(a4), y4)
call check_r4(x4, transpose(a4), y4)

! complex kinds and mixed types
do j = 1, 4
    do i = 1, 3
        c4(i,j) = cmplx(real(i - j) / 2, real(i + j) / 4)
    end do
    u4(j) = cmplx(real(j) / 2, real(2 - j))
end do
c8 = c4; u8 = u4
s4 = matmul(c4, u4)
do i = 1, 3
    if (s4(i) /= sum(c4(i,:)*u4)) error stop 6
end do
s8 = matmul(c8, u8)
if (any(cmplx(s8, kind=4) /= s4)) error stop 7
s4 = matmul(real(i4, 4), u4)
do i = 1, 3
    if (s4(i) /= sum(real(i4(i,:), 4)*u4)) error stop 8
end do
s4 = matmul(c4, real(v4, 4))
do i = 1, 3
    if (s4(i) /= sum(c4(i,:)*real(v4, 4))) error stop 9
end do
y4(1:3) = matmul(real(i4, 4), x4(1:4))
do i = 1, 3
    if (y4(i) /= sum(real(i4(i,:), 4)*x4(1:4))) error stop 10
end do

! logical
la = reshape([.true., .false., .false., .false., .true., .false., &
              .true., .false., .false., .false., .false., .false.], [3,4])
lv = [.false., .true., .false., .true.]
lr = matmul(la, lv)
if (lr(1) .or. .not. lr(2) .or. lr(3)) error stop 11
lv = [.true., .false., .true., .false.]
lr = matmul(la, lv)
if (.not. lr(1) .or. lr(2) .or. lr(3)) error stop 12
lb(:,1) = lv; lb(:,2) = .not. lv
lm = matmul(la, lb)
do j = 1, 2
    do i = 1, 3
        if (lm(i,j) .neqv. any(la(i,:) .and. lb(:,j))) error stop 13
    end do
end do

! zero-size dimensions (sizes known only at run time)
n = 3; m = 0
allocate(wa(n,m), va(m), ra(n))
ra = 1
ra = matmul(wa, va)
if (size(ra) /= 3) error stop 14
if (any(ra /= 0)) error stop 15
deallocate(wa, va, ra)
n = 0; m = 4
allocate(wa(n,m), va(m), ra(n))
va = 1
ra = matmul(wa, va)
if (size(ra) /= 0) error stop 16
deallocate(wa, va, ra)

! allocatable, assumed-shape and pointer operands
n = 5; m = 3
allocate(wa(n,m), va(m), ra(n))
do j = 1, m
    do i = 1, n
        wa(i,j) = real(i*j - 4, 8) / 8
    end do
end do
va = [1.5d0, -2d0, 0.25d0]
ra = matmul(wa, va)
call check_r8(ra, wa, va)
ra = 0
call mv(wa, va, ra)
call check_r8(ra, wa, va)
do j = 1, 4
    do i = 1, 4
        wt(i,j) = real(i - j, 8) / 2
    end do
    vt(j) = real(j, 8) / 4
end do
wp => wt; vp => vt
sv = matmul(wp, vp)
call check_r8(sv, wt, vt)
sv(1:2) = matmul(wp(2:3, 1:4:2), vp(3:4))
call check_r8(sv(1:2), wt(2:3, 1:4:2), vt(3:4))

! results that alias an operand
sq = wt; sv = vt
sref = matmul(sq, sv)
sv = matmul(sq, sv)
if (any(sv /= sref)) error stop 17
sv = vt
sq(:,1) = matmul(sq, sv)
if (any(sq(:,1) /= sref)) error stop 18
sq = wt
smat = matmul(wt, wt)
sq = matmul(sq, sq)
if (any(sq /= smat)) error stop 20

! matrix-matrix: real, strided and transposed operands
do j = 1, 2
    do i = 1, 8
        q4(i,j) = real(mod(3*i + j, 5) - 2) / 2
    end do
end do
p4 = matmul(a4, q4)
do j = 1, 2
    call check_r4(p4(:,j), a4, q4(:,j))
end do
p4(1:3,:) = matmul(a4(1:6:2, :), q4)
do j = 1, 2
    call check_r4(p4(1:3,j), a4(1:6:2, :), q4(:,j))
end do
d8 = matmul(c8, transpose(cmplx(real(i4(1:2,:), 8), kind=8)))
do j = 1, 2
    do i = 1, 3
        if (d8(i,j) /= sum(c8(i,:)*real(i4(j,:), 8))) error stop 23
    end do
end do
smat = matmul(transpose(wt), wt)
do j = 1, 4
    do i = 1, 4
        if (smat(i,j) /= sum(wt(:,i)*wt(:,j))) error stop 21
    end do
end do

! operands with lower bounds other than 1
allocate(la4(0:2, -1:0), lv4(5:6), lc4(2:3, 0:1), lr4(3), lm4(3,2))
do j = -1, 0
    do i = 0, 2
        la4(i,j) = real(i + 3*(j + 1) + 1)
    end do
end do
lv4(5) = 1; lv4(6) = -1
lc4(2,0) = 1; lc4(3,0) = 0; lc4(2,1) = 2; lc4(3,1) = 1
lr4 = matmul(la4, lv4)
if (any(lr4 /= [-3., -3., -3.])) error stop 24
lm4 = matmul(la4, lc4)
if (any(lm4 /= reshape([1., 2., 3., 6., 9., 12.], [3,2]))) error stop 25

! larger random data, compared with the same sums in the same order
call random_number(rnd); call random_number(rv)
rr = matmul(rnd, rv)
rref = 0
do j = 1, 23
    do i = 1, 37
        rref(i) = rref(i) + rnd(i,j)*rv(j)
    end do
end do
if (maxval(abs(rr - rref)) > 1e-4) error stop 22
print *, "ok"

contains

subroutine mv(w, v, r)
    real(8), intent(in) :: w(:,:), v(:)
    real(8), intent(out) :: r(:)
    r = matmul(w, v)
end subroutine

subroutine check_i4(r, w, v)
    integer, intent(in) :: r(:), w(:,:), v(:)
    integer :: i, k, s
    do i = 1, size(w, 1)
        s = 0
        do k = 1, size(w, 2)
            s = s + w(i,k)*v(k)
        end do
        if (r(i) /= s) error stop 30
    end do
end subroutine

subroutine check_r4(r, w, v)
    real(4), intent(in) :: r(:), w(:,:), v(:)
    integer :: i, k
    real(4) :: s
    if (size(r) /= size(w, 1)) error stop 31
    do i = 1, size(w, 1)
        s = 0
        do k = 1, size(w, 2)
            s = s + w(i,k)*v(k)
        end do
        if (r(i) /= s) error stop 32
    end do
end subroutine

subroutine check_r8(r, w, v)
    real(8), intent(in) :: r(:), w(:,:), v(:)
    integer :: i, k
    real(8) :: s
    if (size(r) /= size(w, 1)) error stop 33
    do i = 1, size(w, 1)
        s = 0
        do k = 1, size(w, 2)
            s = s + w(i,k)*v(k)
        end do
        if (r(i) /= s) error stop 34
    end do
end subroutine

end program
