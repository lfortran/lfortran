program gpu_metal_208
! A do concurrent that both contains a block and captures a derived type the
! device cannot lay out. The offload pass used to move the block into an
! unattached kernel scope and then decline the launch, leaving the host loop
! with a BlockCall whose symbol was gone. The loop must stay on the host and
! still compute the right answer.
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
        a(i) = real(i) * s
    end block
end do

do i = 1, 8
    if (abs(a(i) - real(i) * 2.0) > 1e-6) error stop
end do
print *, "PASSED"
end program
