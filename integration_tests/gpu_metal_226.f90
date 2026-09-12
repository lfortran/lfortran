! A POINTER array passed as a `do concurrent` kernel argument.
! Allocatable and pointer arrays are both descriptor-backed, but the GPU
! kernel-argument marshalling only recognised `Allocatable(Array(...))`,
! so `Pointer(Array(...))` either asserted or fell through to the
! non-descriptor path and handed the descriptor itself to the device as
! if it were the data buffer. Both rank 1 and rank 3 are covered.
program gpu_metal_226
implicit none

real, allocatable, target :: a(:)
real, allocatable, target :: b(:,:,:)
real, pointer :: v(:)
real, pointer :: w(:,:,:)
real :: out1(4)
real :: out3(2,3,4)
integer :: i, j, k
integer :: p, q, r

allocate(a(4))
do i = 1, 4
    a(i) = real(i)
end do
v => a

out1 = 0.0
do concurrent(p = 1:4)
    out1(p) = 2.0*v(p)
end do
do i = 1, 4
    if (abs(out1(i) - 2.0*real(i)) > 1.0e-5) error stop "wrong rank-1 value"
end do
print *, out1

allocate(b(2,3,4))
do k = 1, 4
    do j = 1, 3
        do i = 1, 2
            b(i,j,k) = real(i + 10*j + 100*k)
        end do
    end do
end do
w => b

out3 = 0.0
do concurrent(q = 1:4, r = 1:3)
    out3(1,r,q) = 3.0*w(1,r,q)
    out3(2,r,q) = 3.0*w(2,r,q)
end do
do k = 1, 4
    do j = 1, 3
        do i = 1, 2
            if (abs(out3(i,j,k) - 3.0*real(i + 10*j + 100*k)) > 1.0e-2) then
                error stop "wrong rank-3 value"
            end if
        end do
    end do
end do
print *, out3(1,1,1), out3(2,3,4)

deallocate(a)
deallocate(b)
end program
