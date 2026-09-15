program gpu_metal_363
! Expected to fail on the GPU backends with bounds checking on, built
! without --realloc-lhs-arrays. The loop writes t(i) only for odd i, under
! an `if`. Without the option an assignment does not allocate its left-hand
! side, so the launch reports that t(3)%v, which the loop writes, is not
! allocated. The even elements, which the loop does not write, are not
! checked.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
type(tt) :: t(4)
integer :: i, n

c = [1.0, 2.0, 3.0, 4.0, 5.0]
n = 3
allocate(t(1)%v(n))
do concurrent (i = 1:4)
    if (mod(i, 2) == 1) t(i) = f(c(1:n))
end do
print *, allocated(t(3)%v)

contains

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

end program
