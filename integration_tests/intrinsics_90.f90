module intrinsics_90_module

contains
   
subroutine tester(arr)
real(8) :: arr(:,:)

integer :: n
real(8) :: vec(size(arr,1))

vec = huge(1.d0)

do n = 1, size(arr,2)
    vec = min(arr(:,n),vec)
end do

print *, vec
if (any(abs(vec - (-13.9d0)) > 1d-9)) error stop
    
end subroutine tester

end module intrinsics_90_module

program intrinsics_90
use intrinsics_90_module
implicit none

real(8) :: arr(3,3)

arr = -13.9d0

call tester(arr)
end program
