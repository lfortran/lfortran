program gpu_metal_209
! Nested BLOCK inside a do concurrent that captures a derived type the device
! cannot lay out. The inner block reads an outer-block local. A declined
! launch used to restore a snapshot whose inner Vars still pointed at the
! original Block, which had been moved into an unattached kernel scope.
implicit none

type :: label_t
    character(len=:), allocatable :: name
    real :: scale
end type

type(label_t) :: m
real :: a(8)
integer :: i

m%name = "scale by two"
m%scale = 2.0

do concurrent (i = 1:8)
    block
        real :: s
        s = m%scale
        block
            a(i) = real(i) * s
        end block
    end block
end do

do i = 1, 8
    if (abs(a(i) - real(i) * 2.0) > 1e-6) error stop
end do
print *, "PASSED"
end program
