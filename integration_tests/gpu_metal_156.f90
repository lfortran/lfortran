program gpu_metal_156
! Test derived-type array component inside do concurrent.
! The gpu_offload pass must elementize StructInstanceMember references
! with array type so that the Metal shader indexes element-by-element.
implicit none
type :: t
    real :: b(3)
end type
type(t) :: x
integer :: i
real :: z(3)

x%b(1) = 10.0
x%b(2) = 20.0
x%b(3) = 30.0

z = 0.0

do concurrent (i = 1:1)
    z = x%b
end do

if (abs(z(1) - 10.0) > 1.0e-6) error stop
if (abs(z(2) - 20.0) > 1.0e-6) error stop
if (abs(z(3) - 30.0) > 1.0e-6) error stop
print *, "ok"
end program
