! An `error stop` reached on one lane of an iteration whose array statement
! the lanes of a SIMD-group share (gpu_vector_lanes.cpp): the lane records
! it and returns, the other lanes return once the statement is done, and
! the program stops as it does on the CPU.
module gpu_metal_473_m
implicit none
contains
    elemental real function checked_sqrt(x)
        real, intent(in) :: x
        if (x < 0) error stop "negative"
        checked_sqrt = sqrt(x)
    end function
end module

program gpu_metal_473
use gpu_metal_473_m
implicit none
real :: x(8, 4), y(8, 4)
integer :: j
x = 4
x(5, 3) = -1
do concurrent (j = 1:4)
    y(:, j) = checked_sqrt(x(:, j))
    y(:, j) = y(:, j) + 1
end do
print *, sum(y)
end program
