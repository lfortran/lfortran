module gpu_metal_479_m
implicit none
contains
    pure real function first(a)
        real, intent(in) :: a(:)
        first = a(1)
    end function
end module
program gpu_metal_479
use gpu_metal_479_m
implicit none
integer, parameter :: n = 32
real :: t(16), y(n)
integer :: j
do concurrent (j = 1:n)
    t = real(j)
    associate (result => y(j))
        result = first(t)
    end associate
end do
do j = 1, n
    if (y(j) /= j) error stop 1
end do
print *, "ok"
end program
