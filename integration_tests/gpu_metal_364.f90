program gpu_metal_364
! Expected to fail with bounds checking on, built without
! --realloc-lhs-arrays. The offloaded loop assigns an array to the component
! t(i)%v itself, and t(2)%v, which the loop writes, is not allocated.
! Without the option an assignment does not allocate its left-hand side, so
! the launch reports that t(2)%v is not allocated, as bounds checking does
! for the same loop on the CPU, instead of losing the write.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(3)
integer :: i

c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
allocate(t(1)%v(3), t(3)%v(3))
do concurrent (i = 1:2)
    t(i)%v = c(i:i+2)
end do
print *, allocated(t(2)%v)

end program
