program format_hollerith_05
implicit none

! Parentheses among the characters of a Hollerith edit descriptor do not
! close or open a format group.

character(len=64) :: str

write (str, "(i2, 2ha), i3)") 1, 42
if (str /= " 1a) 42") error stop

write (str, "(i2, 2h(a, i3)") 1, 42
if (str /= " 1(a 42") error stop

write (str, 100) 1, 42
if (str /= " 1a) 42") error stop

write (str, 200) 1, 42
if (str /= " 1(a 42") error stop

write (str, 300) 1, 42
if (str /= " 1a) 42") error stop

100 format (i2, 2ha), i3)
200 format (i2, 2h(a, i3)
300 format ((i2), 2ha), i3)
end program format_hollerith_05
