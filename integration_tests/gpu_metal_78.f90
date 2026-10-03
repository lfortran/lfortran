program gpu_metal_78
! Test that do concurrent with many allocatable arrays (>31 Metal buffers
! before packing) compiles and runs correctly on the Metal GPU backend.
! Each allocatable 3D array generates 1 data buffer + 3 dimension parameters.
! 8 arrays * 4 + 1 scalar = 33, which exceeds Metal's 31-buffer limit
! unless scalar parameters are packed into a single struct buffer.
implicit none
integer :: n, i
real, allocatable :: a(:,:,:), b(:,:,:), c(:,:,:), d(:,:,:)
real, allocatable :: e(:,:,:), f(:,:,:), g(:,:,:), r(:,:,:)
n = 2
allocate(a(n,n,n), b(n,n,n), c(n,n,n), d(n,n,n))
allocate(e(n,n,n), f(n,n,n), g(n,n,n), r(n,n,n))
a = 1.0; b = 2.0; c = 3.0; d = 4.0
e = 5.0; f = 6.0; g = 7.0; r = 0.0
do concurrent(i = 1:n)
    r(1,1,i) = a(1,1,i) + b(1,1,i) + c(1,1,i) + d(1,1,i) &
             + e(1,1,i) + f(1,1,i) + g(1,1,i)
end do
if (abs(r(1,1,1) - 28.0) > 1e-6) error stop
if (abs(r(1,1,2) - 28.0) > 1e-6) error stop
print *, "PASS"
end program
