! Derived types holding fp64 data must NOT be offloaded to Metal: MSL has no
! 64-bit floating point type, so the kernel would stride the host buffer at
! the wrong element size. Every loop below has to stay on the CPU and still
! produce the exact fp64 result.
program gpu_metal_220
implicit none

type :: base_t
    double precision :: v
end type

type, extends(base_t) :: derived_t
    integer :: k
end type

type :: alloc_t
    double precision, allocatable :: u(:,:)
end type

type :: inner_t
    double precision :: v
end type

type :: nest_t
    type(inner_t) :: g(2)
end type

type :: mat_t
    double precision :: m(2,2)
    double precision :: r(2,2)
end type

type :: side_t
    double precision :: w
end type

type :: ptr_t
    double precision :: v
    type(side_t), pointer :: nx => null()
end type

type(base_t), allocatable :: s(:)
type(derived_t), allocatable :: d(:)
type(alloc_t) :: c
type(nest_t) :: n
type(mat_t) :: mm(2)
type(ptr_t), allocatable :: pp(:)
integer :: i, j

! 1. scalar double precision component
allocate(s(4))
do concurrent (i = 1:4)
    s(i)%v = 1.0d0 * i
end do
if (abs(sum(s(:)%v) - 10.0d0) > 1.0d-12) error stop "gpu_metal_220: scalar dp component"

! 2. allocatable double precision array component
allocate(c%u(2,2))
do concurrent (i = 1:2, j = 1:2)
    c%u(i,j) = 1.0d0 * (i + j)
end do
if (abs(sum(c%u) - 12.0d0) > 1.0d-12) error stop "gpu_metal_220: allocatable dp component"

! 3. double precision component inherited through EXTENDS
allocate(d(4))
do concurrent (i = 1:4)
    d(i)%v = 2.0d0 * i
    d(i)%k = i
end do
if (abs(sum(d(:)%v) - 20.0d0) > 1.0d-12) error stop "gpu_metal_220: inherited dp component"
if (sum(d(:)%k) /= 10) error stop "gpu_metal_220: inherited integer component"

! 5. array-of-derived-type component with fp64 inside
do concurrent (i = 1:2)
    n%g(i)%v = 3.0d0 * i
end do
if (abs(n%g(1)%v - 3.0d0) > 1.0d-12) error stop "gpu_metal_220: nested array component 1"
if (abs(n%g(2)%v - 6.0d0) > 1.0d-12) error stop "gpu_metal_220: nested array component 2"

! 6. matmul over fp64 matrix components
mm(1)%m = 1.0d0
mm(2)%m = 2.0d0
do concurrent (i = 1:2)
    mm(i)%r = matmul(mm(i)%m, mm(i)%m)
end do
if (abs(sum(mm(1)%r) - 8.0d0) > 1.0d-12) error stop "gpu_metal_220: matmul component 1"
if (abs(sum(mm(2)%r) - 32.0d0) > 1.0d-12) error stop "gpu_metal_220: matmul component 2"

! 8c. pointer-to-derived component alongside an fp64 scalar
allocate(pp(4))
do concurrent (i = 1:4)
    pp(i)%v = 4.0d0 * i
end do
if (abs(sum(pp(:)%v) - 40.0d0) > 1.0d-12) error stop "gpu_metal_220: pointer component"

print *, "gpu_metal_220 ok"
end program
