! The type-level `size(...)` of `matmul` and of `transpose` was answered
! with the size of the intrinsic's first array argument.
!
! `make_ArraySize_t_util` answers a type-level `size(...)` of an
! `IntrinsicArrayFunction` by replacing the call with its first array
! argument, which is right for the intrinsics whose result has the shape
! of their argument.  `matmul` and `transpose` are not among them: the
! type-level size of `matmul(a, v)` with `a(n,k)` and `v(k)` came back as
! `n*k` rather than `n`, `size(matmul(a, b), 2)` came back as `size(a, 2)`
! rather than `size(b, 2)`, and `size(transpose(a), 1)` came back as
! `size(a, 1)` rather than `size(a, 2)`.
!
! Neither intrinsic carries dimensions of its own in its result type, so
! the shape has to be derived from the operands, which is what
! `array_intrinsic_shape_size` now does: matmul is (n,k)x(k,m) -> (n,m),
! (n,k)x(k) -> (n) and (k)x(k,m) -> (m), and transpose reverses the two
! extents of its argument.
!
! The automatic arrays below are declared with exactly those expressions,
! so their bounds were wrong.  The GPU workspace machinery sizes a
! device-side buffer from the same expression, so a device function whose
! array-constructor result contains a matmul got a buffer with the wrong
! extent; both `do concurrent` loops over such a function offload here,
! as does the `size(b, 1)` fence.
module gpu_metal_261_mod
implicit none

contains

    ! (n,k) x (k) -> (n): a matmul inside an array constructor, which is
    ! how a device function's workspace comes to be sized this way.
    pure function ctor_matvec(a, v, k) result(r)
    real, intent(in) :: a(:,:)
    real, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    r = [real(k), matmul(a, v), -real(k)]
    end function

    ! (k) x (k,m) -> (m).
    pure function ctor_vecmat(v, b, k) result(r)
    real, intent(in) :: v(:)
    real, intent(in) :: b(:,:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    r = [real(k), matmul(v, b)]
    end function

    subroutine run_ctor_matvec(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = ctor_matvec(a, v, jj)
    end do
    end subroutine

    subroutine run_ctor_vecmat(o, v, b, nn)
    real, intent(out) :: o(:,:)
    real, intent(in) :: v(:), b(:,:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = ctor_vecmat(v, b, jj)
    end do
    end subroutine

    ! A loop that has always been offloadable, so the kernel count cannot
    ! be met merely because GPU offload stopped happening altogether.
    subroutine run_fence(b, c)
    real, intent(in) :: b(:)
    real, intent(out) :: c(:)
    integer :: jj
    do concurrent (jj = 1:size(b, 1))
        c(jj) = 3.0 * b(jj) - 1.0
    end do
    end subroutine

    ! An automatic array whose whole-array extent is `size(matmul(a, v))`,
    ! which is `n`, not `n*k`.
    function auto_matvec(a, v) result(n)
    real, intent(in) :: a(:,:)
    real, intent(in) :: v(:)
    integer :: n
    real :: t(size(matmul(a, v)))
    n = size(t)
    end function

    ! (n,k) x (k,m): dimension 2 of the result is `size(b, 2)`.
    function auto_matmat(a, b, d1, d2) result(ok)
    real, intent(in) :: a(:,:)
    real, intent(in) :: b(:,:)
    integer, intent(in) :: d1, d2
    logical :: ok
    real :: t(size(matmul(a, b), 1), size(matmul(a, b), 2))
    t = matmul(a, b)
    ok = size(t, 1) == d1 .and. size(t, 2) == d2 .and. size(t) == d1 * d2
    end function

    ! `transpose` reverses the two extents.
    function auto_transpose(a, d1, d2) result(ok)
    real, intent(in) :: a(:,:)
    integer, intent(in) :: d1, d2
    logical :: ok
    real :: t(size(transpose(a), 1), size(transpose(a), 2))
    t = transpose(a)
    ok = size(t, 1) == d1 .and. size(t, 2) == d2
    end function

    ! The dimension is only known at run time, so the extent is a `merge`
    ! of the two candidate extents rather than one of them.
    function auto_matmat_dim(a, b, d) result(n)
    real, intent(in) :: a(:,:)
    real, intent(in) :: b(:,:)
    integer, intent(in) :: d
    integer :: n
    real :: t(size(matmul(a, b), d))
    n = size(t)
    end function

end module

program gpu_metal_261
use gpu_metal_261_mod
implicit none

integer, parameter :: nrow = 3, ncol = 2, ncb = 4, nn = 4
real :: a(nrow, ncol), b(ncol, ncb), v(ncol)
real :: o1(nrow + 2, nn), o2(ncb + 1, nn)
real :: bb(5), cc(5)
real :: expected
integer :: i, j, k

do i = 1, nrow
    do k = 1, ncol
        a(i, k) = real(i) + 10.0 * real(k)
    end do
end do
do i = 1, ncol
    do k = 1, ncb
        b(i, k) = real(i) * 2.0 - real(k)
    end do
end do
do i = 1, ncol
    v(i) = real(i) * 3.0 - 1.0
end do

! (n,k) x (k) inside an array constructor, on the device.
o1 = 0.0
call run_ctor_matvec(o1, a, v, nn)
print *, o1
do j = 1, nn
    if (abs(o1(1, j) - real(j)) > 1.0e-4) error stop "matvec: head"
    if (abs(o1(nrow + 2, j) + real(j)) > 1.0e-4) error stop "matvec: last"
    do i = 1, nrow
        expected = 0.0
        do k = 1, ncol
            expected = expected + a(i, k) * v(k)
        end do
        if (abs(o1(i + 1, j) - expected) > 1.0e-4 * max(1.0, abs(expected))) then
            print *, "matvec", i, j, o1(i + 1, j), expected
            error stop "matvec: body"
        end if
    end do
end do

! (k) x (k,m) inside an array constructor, on the device.
o2 = 0.0
call run_ctor_vecmat(o2, v, b, nn)
print *, o2
do j = 1, nn
    if (abs(o2(1, j) - real(j)) > 1.0e-4) error stop "vecmat: head"
    do i = 1, ncb
        expected = 0.0
        do k = 1, ncol
            expected = expected + v(k) * b(k, i)
        end do
        if (abs(o2(i + 1, j) - expected) > 1.0e-4 * max(1.0, abs(expected))) then
            print *, "vecmat", i, j, o2(i + 1, j), expected
            error stop "vecmat: body"
        end if
    end do
end do

do j = 1, 5
    bb(j) = real(j)
end do
cc = 0.0
call run_fence(bb, cc)
print *, cc
do j = 1, 5
    if (abs(cc(j) - (3.0 * real(j) - 1.0)) > 1.0e-5) error stop "fence loop"
end do

! The declared bounds of an automatic array, which is where the
! type-level size of these intrinsics is used.
print *, auto_matvec(a, v)
if (auto_matvec(a, v) /= nrow) error stop "auto: matvec extent"
if (.not. auto_matmat(a, b, nrow, ncb)) error stop "auto: matmat extents"
if (.not. auto_transpose(a, ncol, nrow)) error stop "auto: transpose extents"
print *, auto_matmat_dim(a, b, 1), auto_matmat_dim(a, b, 2)
if (auto_matmat_dim(a, b, 1) /= nrow) error stop "auto: matmat dim 1"
if (auto_matmat_dim(a, b, 2) /= ncb) error stop "auto: matmat dim 2"

print *, "ok"

end program
