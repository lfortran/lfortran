program gpu_metal_615
! A device routine whose array-expression temporary is allocated twice,
! once with a default-kind extent and once (reallocation on assignment)
! with an extent of the descriptor index kind
implicit none
real :: v(4), o(5)
integer :: i
v = [1., 2., 3., 4.]
do concurrent (i = 1:5)
    o(i) = f(v) + i
end do
print *, o
do i = 1, 5
    if (.not. (abs(o(i) - (20 + i)) <= 1e-5)) error stop
end do
contains

pure real function g(x)
    real, intent(in) :: x(:)
    g = sum(x)
end function

pure real function f(x)
    real, intent(in) :: x(:)
    real, allocatable :: w(:)
    w = x
    f = g(2 * w)
end function

end program
