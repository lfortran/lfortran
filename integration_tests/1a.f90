program read_59
implicit none

character(20) :: input
character(5) :: a, b
integer :: i
input = "hello , ,  world"

read(input, *) a, b
print *, (len(b))
do i = 1, len(b)
    print *, iachar(b(i:i))
end do
end program read_59