program read_53
implicit none

character(20) :: input
character(5) :: a, b

input = "hello world"

read(input, *) a, b

if (b/="world") error stop

end program read_53