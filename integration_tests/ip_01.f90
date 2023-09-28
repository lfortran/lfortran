program ip_01
use iso_fortran_env
implicit none
integer, parameter :: IP = int32
integer(IP) :: n_small = 2_IP
print *, n_small
if (n_small /= 2_IP) error stop
end program
