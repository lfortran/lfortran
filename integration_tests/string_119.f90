program string_119
implicit none

character(kind=4,len=3) :: low = 4_"abc"
character(kind=4,len=3) :: high = 4_"def"

if (min(low, high) /= 4_"abc") error stop 1
if (max(low, high) /= 4_"def") error stop 2
if (min(high, 4_"ace") /= 4_"ace") error stop 3
if (max(high, 4_"ace") /= 4_"def") error stop 4

end program
