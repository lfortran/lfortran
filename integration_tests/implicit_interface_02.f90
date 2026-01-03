program main

integer :: b(3) = [10, 20, 30]
integer :: n = 3

call driver(implicit_interface_check, b)

contains

subroutine driver(fnc, arr)
integer, intent(in) :: arr(3)
print *, abs(-1.0) ! This loads the runtime library(bug fixed)
call fnc(arr)
end subroutine

subroutine implicit_interface_check(arr1)
integer, intent(in) :: arr1(3)
if (arr1(1) /= 10) error stop
if (arr1(2) /= 20) error stop
if (arr1(3) /= 30) error stop
end subroutine

end program
