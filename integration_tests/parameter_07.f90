program parameter_07
integer, parameter :: n = 2
complex ::  sum
real, parameter :: d(3 : n+3) = [2.48574089138753566e-5,1.05142378581721974, -3.45687097222016235]
print *, d(3)
if (abs(d(3) - 2.48574089138753566e-5) > 1e-6) error stop
print *, d(4)
if (abs(d(4) - 1.05142378581721974) > 1e-6) error stop
print *, d(5)
if (abs(d(5) + 3.45687097222016235) > 1e-6) error stop
end program 
