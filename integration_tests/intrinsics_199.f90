program intrinsics_199
integer, parameter :: n = 10
logical :: x(n)
integer :: res

x = .false.
x(1:5) = .true.
res = count(x, 1)
print *, res
if (res /= 5) error stop
end program
