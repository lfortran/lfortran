program intrinsics_422
! Test this_image() intrinsic
implicit none
integer :: id
id = this_image()
if (id /= 1) error stop
print *, id
end program
