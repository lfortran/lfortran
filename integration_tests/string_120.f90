program string_120
implicit none

character(kind=4,len=2) :: word

word = 4_"x "
if (trim(word) /= 4_"x") error stop 1
if (len(trim(word)) /= 1) error stop 2

end program
