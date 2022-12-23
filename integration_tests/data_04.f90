program data_04
integer, parameter :: dp = kind(0.d0)
double precision dmach(3)
data dmach(1) /2.22044604926d-16/
data dmach(2) /2.22507385852d-308/
data dmach(3) /1.79769313485d+308/
print *, dmach
if (abs(dmach(1) - 2.22044604926d-16) > 1e-15_dp) error stop
if (abs(dmach(2) - 2.22507385852d-308) > 1e-15_dp) error stop
if (abs(dmach(3) - 1.79769313485d+308) > 1e-15_dp) error stop
end program
