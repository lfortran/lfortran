program write_27
implicit none
character(len=10) :: s
character(len=3) :: t
integer :: val

! Test write with unit= keyword pointing to a character variable (internal file)
val = 42
write(unit=s, fmt="(i10)") val
if (trim(adjustl(s)) /= "42") error stop

! Test with a shorter string
write(unit=t, fmt="(i3)") 7
if (trim(adjustl(t)) /= "7") error stop

print *, trim(adjustl(s))
print *, trim(adjustl(t))
end program
