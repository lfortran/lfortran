program test
implicit none

character(20) :: input
character(5) :: a, b

input = "hello world"

read(input, *) a, b

print *, a
print *, b

end program