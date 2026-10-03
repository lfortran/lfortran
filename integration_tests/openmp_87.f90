program openmp_87
! More arrays in private and firstprivate clauses: an allocatable that is
! not allocated at entry, pointer arrays, an assumed-shape dummy, and
! private arrays on parallel sections and teams
use omp_lib
implicit none
integer :: i, j, k, nbad, s, s1, s2, arr(4), x(10)
integer, allocatable :: work(:)
integer, target :: t(4), u(4)
integer, pointer :: p(:)

call omp_set_num_threads(4)

! an allocatable that is not allocated at entry stays unallocated in every
! thread, and what the region allocates never reaches the original
nbad = 0
!$omp parallel do private(work, j, k) reduction(+:nbad)
do i = 1, 100
    if (allocated(work)) nbad = nbad + 1
    allocate(work(i))
    work = i
    k = 0
    do j = 1, i
        k = k + work(j)
    end do
    if (k /= i*i) nbad = nbad + 1
    deallocate(work)
end do
!$omp end parallel do
if (nbad /= 0) error stop "unallocated private allocatable"
if (allocated(work)) error stop "private allocatable allocated after the region"

! an allocation the region does not free is not visible after it
!$omp parallel private(work)
allocate(work(3))
work = 1
!$omp end parallel
if (allocated(work)) error stop "private allocatable leaked out of the region"

! a private pointer array can be pointed at other storage, and the pass
! must not free its target
t = 3
u = 7
p => t
s = 0
!$omp parallel private(p) reduction(+:s)
p => u
s = s + p(1)
!$omp end parallel
if (s /= 7*omp_get_max_threads()) error stop "private pointer array"
if (.not. associated(p, t)) error stop "private pointer array association changed"
if (t(1) /= 3 .or. u(4) /= 7) error stop "private pointer array target changed"

! a firstprivate pointer array is associated with the original target
nbad = 0
!$omp parallel firstprivate(p) reduction(+:nbad)
if (.not. associated(p, t)) nbad = nbad + 1
if (p(4) /= 3) nbad = nbad + 1
!$omp end parallel
if (nbad /= 0) error stop "firstprivate pointer array"

! private array on parallel sections
arr = 5
s1 = 0
s2 = 0
!$omp parallel sections private(arr) shared(s1, s2)
!$omp section
arr = 1
s1 = arr(1) + arr(4)
!$omp section
arr = 2
s2 = arr(1) + arr(4)
!$omp end parallel sections
if (s1 /= 2 .or. s2 /= 4) error stop "private array on parallel sections"
if (arr(1) /= 5 .or. arr(4) /= 5) error stop "parallel sections private array written back"

! private array on teams
nbad = 0
!$omp teams num_teams(2) private(arr) reduction(+:nbad)
arr = 9
if (arr(2) /= 9) nbad = nbad + 1
!$omp end teams
if (nbad /= 0) error stop "private array on teams"
if (arr(1) /= 5 .or. arr(4) /= 5) error stop "teams private array written back"

x = 5
call assumed_shape(x)
if (x(1) /= 5 .or. x(10) /= 5) error stop "private assumed-shape array written back"
print *, "ok"

contains

subroutine assumed_shape(a)
    integer, intent(inout) :: a(:)
    integer :: i, nbad
    nbad = 0
    !$omp parallel do private(a) reduction(+:nbad)
    do i = 1, 1000
        a = i
        if (a(1) + a(10) /= 2*i) nbad = nbad + 1
    end do
    !$omp end parallel do
    if (nbad /= 0) error stop "private assumed-shape array shared"
end subroutine

end program
