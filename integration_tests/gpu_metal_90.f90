program gpu_metal_90
! Test: indexing allocatable array member of a derived type array
! in do concurrent (Metal GPU offload). The allocatable data for
! each struct element must be flattened into a separate device buffer.
implicit none

type :: t
    real, allocatable :: v(:)
end type

type(t) :: a(2)
real :: r(2)
integer :: i

allocate(a(1)%v(1), a(2)%v(1))
a(1)%v(1) = 1.0
a(2)%v(1) = 2.0

do concurrent (i = 1:2)
    r(i) = a(i)%v(1)
end do

if (abs(r(1) - 1.0) > 0.001) error stop
if (abs(r(2) - 2.0) > 0.001) error stop

print *, "ok"
end program
