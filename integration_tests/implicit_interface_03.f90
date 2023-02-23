program main

integer :: b(3) = [10, 20, 30]
integer :: n = 3

call driver(implicit_interface_check, b, n)

contains

subroutine driver(fnc, arr, m)
integer, intent(in) :: m, arr(m) ! This fixes the array length issue (BindC changes)
print *, abs(-1.0) ! This loads the runtime library
call fnc(arr, m)
end subroutine

subroutine implicit_interface_check(arr1, m)
integer, intent(in) :: m, arr1(m)
if (m /= 3) error stop
if (arr1(1) /= 10) error stop
if (arr1(2) /= 20) error stop
if (arr1(3) /= 30) error stop
end subroutine

end program
