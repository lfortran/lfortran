program gpu_metal_358
! Expected to fail on the GPU backends with bounds checking on, built with
! --realloc-lhs-arrays. The kernel gives t(i)%v the extent s(i), which the
! loop writes first, so the host cannot size the component before the
! launch, and the program did not allocate it. The launch has to report the
! unallocated component rather than size it from the value s holds before
! the loop, which would silently truncate the result.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
integer :: s(3)
type(tt) :: t(3)
integer :: i

c = [1.0, 2.0, 3.0, 4.0, 5.0]
s = 1
do concurrent (i = 1:3)
    s(i) = i + 1
    t(i) = f(c(1:s(i)))
end do
print *, size(t(1)%v), size(t(2)%v), size(t(3)%v)

contains

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

end program
