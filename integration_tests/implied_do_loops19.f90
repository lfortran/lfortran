program implied_do_loops19
implicit none
character(*), parameter :: punc = ",; ."
integer, parameter :: lpunc = len(punc)
integer :: i, j
character(4) :: input(lpunc**2)

input = [(("2"//punc(i:i)//"5"//punc(j:j), i=1,lpunc), j=1,lpunc)]

if (size(input) /= 16) error stop
if (input(1) /= "2,5,") error stop
if (input(2) /= "2;5,") error stop
if (input(3) /= "2 5,") error stop
if (input(4) /= "2.5,") error stop
if (input(5) /= "2,5;") error stop
if (input(6) /= "2;5;") error stop
if (input(9) /= "2,5 ") error stop
if (input(13) /= "2,5.") error stop
if (input(16) /= "2.5.") error stop

end program implied_do_loops19
