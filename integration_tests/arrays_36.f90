program arrays_36
use iso_fortran_env, only: int16
integer(int16), allocatable :: result(:)

integer(int16) :: start_, end_, step_
integer(int16) :: i

allocate(result(10))
start_ = 1_int16
end_ = 10_int16

result = [(i, i=start_, end_)]

print *, result
i = 1_int16
do while (i <= 10_int16)
    if (result(i) /= i) error stop
    i = i + 1_int16
end do

end program
