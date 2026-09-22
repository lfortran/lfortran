program openmp_86
! Arrays in private and firstprivate clauses get storage of their own
! in every thread, so writes in the region never reach the original
use omp_lib
implicit none
integer :: i, j, k, nbad, fixed(10)
integer, allocatable :: alloc(:)
real :: grid(2, 3)

call omp_set_num_threads(4)

! private fixed-size array on parallel do
fixed = 5
nbad = 0
!$omp parallel do private(fixed, j, k) reduction(+:nbad)
do i = 1, 1000
    fixed = i
    k = 0
    do j = 1, 10
        k = k + fixed(j)
    end do
    if (k /= 10*i) nbad = nbad + 1
end do
!$omp end parallel do
if (nbad /= 0) error stop "private fixed-size array shared"
if (fixed(1) /= 5 .or. fixed(10) /= 5) error stop "private fixed-size array written back"

! firstprivate fixed-size array starts with the values of the original
nbad = 0
!$omp parallel do firstprivate(fixed) private(j, k) reduction(+:nbad)
do i = 1, 1000
    if (fixed(10) /= 5) nbad = nbad + 1
    fixed(1:9) = i
    k = 0
    do j = 1, 10
        k = k + fixed(j)
    end do
    if (k /= 9*i + 5) nbad = nbad + 1
end do
!$omp end parallel do
if (nbad /= 0) error stop "firstprivate array shared or not copied in"
if (fixed(1) /= 5 .or. fixed(10) /= 5) error stop "firstprivate array written back"

! private allocatable array keeps the bounds of the original
allocate(alloc(-2:7))
alloc = 5
nbad = 0
!$omp parallel do private(alloc, j, k) reduction(+:nbad)
do i = 1, 1000
    if (lbound(alloc, 1) /= -2 .or. ubound(alloc, 1) /= 7) nbad = nbad + 1
    alloc = i
    k = 0
    do j = -2, 7
        k = k + alloc(j)
    end do
    if (k /= 10*i) nbad = nbad + 1
end do
!$omp end parallel do
if (nbad /= 0) error stop "private allocatable array shared"
if (alloc(-2) /= 5 .or. alloc(7) /= 5) error stop "private allocatable array written back"

! firstprivate 2D real array on a parallel region
grid = 5.0
nbad = 0
!$omp parallel firstprivate(grid) private(j, k) reduction(+:nbad)
do j = 1, 200
    if (grid(2, 3) /= 5.0 + real(j - 1)) nbad = nbad + 1
    grid = grid + 1.0
end do
!$omp end parallel
if (nbad /= 0) error stop "firstprivate 2D array shared or not copied in"
if (grid(1, 1) /= 5.0 .or. grid(2, 3) /= 5.0) error stop "firstprivate 2D array written back"

call explicit_shape(fixed, 10)
print *, "ok"

contains

subroutine explicit_shape(a, n)
    integer, intent(in) :: n
    integer, intent(inout) :: a(n)
    integer :: i, j, k, nbad
    nbad = 0
    !$omp parallel do private(a, j, k) reduction(+:nbad)
    do i = 1, 1000
        a = i
        k = 0
        do j = 1, n
            k = k + a(j)
        end do
        if (k /= n*i) nbad = nbad + 1
    end do
    !$omp end parallel do
    if (nbad /= 0) error stop "private explicit-shape array shared"
    if (a(1) /= 5 .or. a(n) /= 5) error stop "private explicit-shape array written back"
end subroutine

end program
