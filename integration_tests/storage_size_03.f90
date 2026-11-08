program storage_size_03
use iso_c_binding, only: c_sizeof
implicit none
integer, parameter :: n = storage_size(1.0_10)
real(10) :: x
x = 1
if (n /= 128) error stop
if (storage_size(x) /= 128) error stop
if (storage_size(x) /= 8*c_sizeof(x)) error stop
end program
