! Tests c_ptr passing and calling as an argument
program bindc_02
use bindc_02b, only: driver
implicit none
print *, "Main program: calling driver()"
call driver()
end
