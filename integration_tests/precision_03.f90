program precision_03
print *, 3.0*3.1
if (abs((3.0*3.1) - 9.29999924) > 1e-6) error stop
end program
