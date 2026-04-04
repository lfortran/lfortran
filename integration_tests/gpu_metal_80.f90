program gpu_metal_80
! Test: size() of allocatable derived-type member inside block in do concurrent
implicit none
type :: t
    integer, allocatable :: x(:)
end type
type(t) :: s
integer :: i
real :: r(3)
allocate(s%x(3))
s%x = [1, 2, 3]
do concurrent (i = 1:3)
    block
        integer :: n
        n = size(s%x)
        r(i) = real(n)
    end block
end do
if (any(r /= 3.0)) error stop
print *, "ok"
end program
