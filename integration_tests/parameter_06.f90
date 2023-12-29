program parameter_06
integer, parameter :: n = 2
complex ::  sum
real, parameter :: d(0 : n) = [2.48574089138753566e-5,1.05142378581721974, -3.45687097222016235]
print *, d(0)
if (abs(d(0) - 2.48574089138753566e-5) > 1e-6) error stop
sum = cmplx(d(0), kind = 8)
print *, sum
if (abs(sum % re - 2.48574089138753566e-5) > 1e-6) error stop
end program 
