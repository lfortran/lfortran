program format_hollerith_04
! The text of an nH edit descriptor is the n characters after the H that
! follows the count, even when the text itself contains an 'h' or 'H'.
implicit none
character(len=20) :: s
write (s, '(2Hxh)')
if (s /= 'xh') error stop 1
write (s, '(2Hhx)')
if (s /= 'hx') error stop 2
write (s, '(5Hhello)')
if (s /= 'hello') error stop 3
write (s, '(1Hh)')
if (s /= 'h') error stop 4
write (s, '(3Hhat)')
if (s /= 'hat') error stop 5
write (s, '(5Haahaa)')
if (s /= 'aahaa') error stop 6
write (s, '(5Hxhxxx)')
if (s /= 'xhxxx') error stop 7
write (s, '(3HaHb)')
if (s /= 'aHb') error stop 8
write (s, '(a,tr1,2Hhh)') 'x'
if (s /= 'x hh') error stop 9
write (s, '(2hxh)')
if (s /= 'xh') error stop 10
write (s, '(8Hthe h, H)')
if (s /= 'the h, H') error stop 11
write (s, 100)
if (s /= 'xh') error stop 12
write (s, 200) 'x'
if (s /= 'x hh') error stop 13
write (s, 300) 7
if (s /= 'h=  7 hh') error stop 14
100 format(2Hxh)
200 format(a,1x,2Hhh)
300 format(2Hh=,i3,3H hh)
print *, 'ok'
end program
