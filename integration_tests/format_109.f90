program format_109
! SP editing must force a plus sign on non-negative values for G editing.
implicit none

character(len=16) :: out

write(out, "(sp,g0)") 10
if (out /= "+10") error stop 1

write(out, "(sp,g0)") -10
if (out /= "-10") error stop 2

write(out, "(sp,g0)") 5.0
if (out /= "+5.00000000") error stop 3

write(out, "(sp,g12.4)") 23.45
if (out /= "  +23.45    ") error stop 4

! SS restores sign suppression
write(out, "(sp,ss,g0)") 10
if (out /= "10") error stop 5

write(out, "(sp,g0)") 0.0
if (out /= "+0.00000000") error stop 6

print *, "ok"
end program format_109
