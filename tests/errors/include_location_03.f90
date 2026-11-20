! The run-time error is in the INCLUDE file, at line 7
include 'include_location_03.inc'

program include_location_03
implicit none
real(8) :: b(5)
call setit(b, 3)
print *, b(3)
end program
