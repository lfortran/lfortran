program gpu_metal_226
! An array constructor whose elements are array-valued expressions is
! materialized into a temporary. Inside a device function that temporary
! would have to be a variable-length array whenever its extent comes
! from an assumed-shape dummy (`matmul(mm, x)`) or from an ALLOCATE with
! a run-time bound (`w`). Metal has neither VLAs nor a heap, so such a
! `do concurrent` must not be offloaded; it runs on the host instead and
! must still produce the right values.
implicit none
integer, parameter :: n = 4
real :: a(8,n)
real, allocatable :: v(:), m(:,:)
integer :: j, k
allocate(v(2), m(2,2))
v = [10.0, 20.0]
m = reshape([1.0, 0.0, 0.0, 1.0], [2,2])
k = 3
a = 0.0
do concurrent (j = 1:n)
    a(:,j) = f(m, v, k)
end do
do j = 1, n
    if (abs(a(1,j) - 0.0) > 1.0e-6) error stop "leading scalar wrong"
    if (abs(a(2,j) - 10.0) > 1.0e-6) error stop "matmul segment wrong"
    if (abs(a(3,j) - 20.0) > 1.0e-6) error stop "matmul segment wrong"
    if (abs(a(4,j) - 7.0) > 1.0e-6) error stop "allocatable segment wrong"
    if (abs(a(5,j) - 7.0) > 1.0e-6) error stop "allocatable segment wrong"
    if (abs(a(6,j) - 7.0) > 1.0e-6) error stop "allocatable segment truncated"
    if (abs(a(7,j) - 0.0) > 1.0e-6) error stop "trailing scalar wrong"
    if (abs(a(8,j) - 0.0) > 1.0e-6) error stop "trailing scalar wrong"
end do
print *, a(:,1)
print *, "ok"
contains
    pure function f(mm, x, kk) result(r)
    real, intent(in) :: mm(:,:), x(:)
    integer, intent(in) :: kk
    real :: r(8)
    real, allocatable :: w(:)
    allocate(w(kk))
    w = 7.0
    r = [0.0, matmul(mm, x), w, 0.0, 0.0]
    end function
end program
