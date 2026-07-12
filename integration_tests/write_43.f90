program write_43
implicit none

integer :: value = 7
character(12) :: buffer(4, 1)
integer :: stat

buffer = "unset"
stat = 0
write(buffer(3:1, 1), "(i12)", iostat=stat) value

if (stat /= -1) error stop
if (trim(buffer(1, 1)) /= "unset") error stop

print *, "PASS"
end program write_43
