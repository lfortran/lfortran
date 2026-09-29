program gpu_metal_359
! Expected to fail on the GPU backends with bounds checking on, built
! without --realloc-lhs-arrays. The kernel gives t(i)%v a size, but without
! the option an assignment does not allocate its left-hand side, so the
! launch reports that t(2)%v, which the loop writes, is not allocated,
! instead of allocating it.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
type(tt) :: t(3)
integer :: i, n

c = [1.0, 2.0, 3.0, 4.0, 5.0]
n = 3
allocate(t(1)%v(n))
do concurrent (i = 1:2)
    t(i) = f(c(1:n))
end do
print *, size(t(1)%v), size(t(2)%v)

contains

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

end program
