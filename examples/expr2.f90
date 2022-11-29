program expr2
implicit none

real :: x

x = (2+3)*5
print *, log_gamma(log_gamma(x) + log_gamma(x + 1)), log_gamma(x)

end program
