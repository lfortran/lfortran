program intrinsics_423
! Test num_images() intrinsic
implicit none
integer :: n
n = num_images()
if (n /= 1) error stop
print *, n
end program
