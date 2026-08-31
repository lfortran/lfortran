program gpu_metal_225
! ALLOCATE statements inside BLOCK and ASSOCIATE constructs of an offloaded
! device function must be recorded, so that array-constructor segments built
! from those allocatables get their extents (and the running offsets of every
! later segment) right.
implicit none
integer, parameter :: n = 4
real :: a(7,n), v(2), m(2,2)
integer :: j
v = [10.0, 20.0]
m = reshape([1.0, 0.0, 0.0, 1.0], [2,2])
a = 0.0
do concurrent (j = 1:n)
    a(:,j) = f(m, v)
end do
do j = 1, n
    if (abs(a(1,j) - 0.0) > 1.0e-6) error stop "segment 1 wrong"
    if (abs(a(2,j) - 10.0) > 1.0e-6) error stop "segment 2 wrong"
    if (abs(a(3,j) - 20.0) > 1.0e-6) error stop "segment 2 wrong"
    if (abs(a(4,j) - 3.0) > 1.0e-6) error stop "block segment wrong"
    if (abs(a(5,j) - 3.0) > 1.0e-6) error stop "block segment truncated"
    if (abs(a(6,j) - 1.0) > 1.0e-6) error stop "associate segment wrong"
    if (abs(a(7,j) - 5.0) > 1.0e-6) error stop "function scope segment wrong"
end do
print *, a(:,1)
print *, "ok"
contains
    pure function f(mm, x) result(r)
    real, intent(in) :: mm(2,2), x(2)
    real :: r(7)
    real, allocatable :: b1(:), b2(:), fence(:)
    ! Function-scope ALLOCATE: already handled, kept here as a fence.
    allocate(fence(1))
    fence = 5.0
    block
        allocate(b1(2))
    end block
    b1 = 3.0
    associate (s => mm(1,1))
        allocate(b2(1))
        b2 = s
    end associate
    ! The matmul segment forces the Metal ArrayConstructor path; the two
    ! allocatable segments after it exercise the running-offset chain.
    r = [0.0, matmul(mm, x), b1, b2, fence]
    end function
end program
