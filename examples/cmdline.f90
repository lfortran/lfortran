program cmdline
integer, parameter :: dp = kind(0.d0)
integer :: x
character(len=32) :: arg
real :: err
if (command_argument_count() /= 1) error stop "Must provide exactly 1 argument"
call get_command_argument(1, arg)
print *, "Got argument:", arg
read(arg,*) x
print *, "Converted value:", x
print *, "sin(x) = ", sin(real(x, dp))
print *, "sin(1) = ", sin(1._dp)
err = abs(sin(real(x, dp)) - 8.41470984807896505e-01_dp)
print *, "err =", err
if (err > 1e-12) then
    print *, "Make sure you call cmdline with the argument: 1"
    error stop "The sin(x) does not agree with the reference"
end if
end program
