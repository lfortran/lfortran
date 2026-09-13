program gpu_metal_206
! A do concurrent loop that reads a derived type with an allocatable scalar
! component. A deferred length character component is a host heap address,
! which no device layout can reproduce, so the loop is not offloaded: it runs
! on the CPU and the compiler warns instead of failing.
implicit none

type :: label_t
    character(len=:), allocatable :: name
    real :: scale
end type

type :: model_t
    type(label_t) :: label
    integer :: offset
end type

type(model_t) :: m
real :: a(8)
integer :: i

m%label%name = "scale by two"
m%label%scale = 2.0
m%offset = 1

do concurrent (i = 1:8)
    a(i) = real(i + m%offset) * m%label%scale
end do

do i = 1, 8
    if (abs(a(i) - real(i + 1) * 2.0) > 1e-6) error stop
end do
print *, "PASSED"
end program
