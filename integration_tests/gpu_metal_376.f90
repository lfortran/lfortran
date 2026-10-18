program gpu_metal_376
! Two writes of a component in one iteration that give it different sizes,
! with the component not allocated before the loop. The host cannot pick
! one of the sizes before the launch, so each value is made on the device
! heap and the last one written is copied into t(i) after the launch.
implicit none
type tt
    real, allocatable :: v(:)
end type
type(tt) :: t(4)
integer :: i
do concurrent (i = 1:4)
    t(i) = f(2, 1.0)
    t(i) = f(3, real(i))
end do
print *, size(t(4)%v), t(4)%v
if (size(t(4)%v) /= 3 .or. any(t(4)%v /= 4.0)) error stop 1
contains
pure function f(n, s) result(r)
    integer, intent(in) :: n
    real, intent(in) :: s
    type(tt) :: r
    allocate(r%v(n))
    r%v = s
end function
end program
