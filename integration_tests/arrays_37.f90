program arrays_37
use iso_fortran_env, only: dp => real64
real(dp), allocatable :: result(:)

real(dp) :: start_, end_, step_
integer :: i

allocate(result(10))
start_ = 1.0_dp
end_ = 10.0_dp
step_ = 1.0_dp

result = [(start_ + (i - 1)*step_, i=1, size(result), 1)]
print *, result
i = 1
do while (i <= size(result))
    if (abs(result(i) - (start_ + (i - 1)*step_)) > 1.0e-15_dp) error stop
    i = i + 1
end do
end program
