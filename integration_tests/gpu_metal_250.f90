! A derived-type component that is itself an array of a derived type
! reaches the shader as a member of the struct handed to the kernel.
! The Metal struct definition used to drop the component's extent and
! declare it as a single element, while a reference to it was still
! emitted as a subscript, so the shader failed to compile with "does not
! provide a subscript operator".
!
! Both loops below must become kernels -- 2 kernels for this program.
module gpu_metal_250_mod
implicit none

type :: inner_t
    real :: c
    integer :: i
end type

type :: base_t
    integer :: tag
end type

type, extends(base_t) :: outer_t
    type(inner_t) :: p(3)
    type(inner_t) :: q(2, 2)
    real :: s
end type

end module

program gpu_metal_250
use gpu_metal_250_mod
implicit none
integer, parameter :: n = 5
type(outer_t) :: o
real :: out(n)
integer :: iout(n)
integer :: j

o%tag = 4
o%p(1)%c = 3.0
o%p(2)%c = 7.0
o%p(3)%c = 11.0
o%p(1)%i = 1
o%p(2)%i = 2
o%p(3)%i = 3
o%q(1, 1)%c = 1.0
o%q(2, 1)%c = 2.0
o%q(1, 2)%c = 3.0
o%q(2, 2)%c = 4.0
o%q(1, 1)%i = 10
o%q(2, 1)%i = 20
o%q(1, 2)%i = 30
o%q(2, 2)%i = 40
o%s = 0.5

out = -1
do concurrent (j = 1:n)
    out(j) = o%p(1)%c * j + o%p(3)%c + o%q(2, 2)%c + o%s
end do
do j = 1, n
    if (abs(out(j) - (3.0*j + 11.0 + 4.0 + 0.5)) > 1.0e-4) error stop "real"
end do

iout = -1
do concurrent (j = 1:n)
    iout(j) = o%p(2)%i * j + o%q(1, 2)%i + o%tag
end do
do j = 1, n
    if (iout(j) /= 2*j + 30 + 4) error stop "integer"
end do

print *, "ok"
end program
