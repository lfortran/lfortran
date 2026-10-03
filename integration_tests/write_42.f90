program write_42
implicit none

integer :: value = 7
character(12) :: buffer(4, 1)

buffer = "unset"
write(buffer(:, 1), "(i12)") value

if (trim(adjustl(buffer(1, 1))) /= "7") error stop
if (trim(buffer(2, 1)) /= "unset") error stop

print *, trim(adjustl(buffer(1, 1)))
end program write_42
