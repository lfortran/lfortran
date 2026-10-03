! Regression fence for the Metal representability walk over derived types:
! types whose components are all 4-byte real/integer ARE representable in
! MSL and must still be offloaded (and produce correct results), including
! when the components are inherited through EXTENDS.
program gpu_metal_221
implicit none

type :: plain_t
    real :: v
    integer :: k
end type

type :: base_t
    real :: b
end type

type, extends(base_t) :: child_t
    real, allocatable :: u(:)
end type

type(plain_t), allocatable :: a(:)
type(child_t) :: c
integer :: i

allocate(a(4))
do concurrent (i = 1:4)
    a(i)%v = 1.0 * i
    a(i)%k = 2 * i
end do
if (abs(sum(a(:)%v) - 10.0) > 1.0e-6) error stop "gpu_metal_221: real(4) component"
if (sum(a(:)%k) /= 20) error stop "gpu_metal_221: integer(4) component"

c%b = 0.5
allocate(c%u(4))
do concurrent (i = 1:4)
    c%u(i) = 1.0 * i
end do
if (abs(sum(c%u) - 10.0) > 1.0e-6) error stop "gpu_metal_221: extends real(4) component"
if (abs(c%b - 0.5) > 1.0e-6) error stop "gpu_metal_221: inherited real(4) component"

print *, "gpu_metal_221 ok"
end program
