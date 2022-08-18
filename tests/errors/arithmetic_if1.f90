program arithmetic_if1
integer :: x, c
x = -3
c = 0
if ("yy") 1, 2, 3
1 c = c + 1
2 c = c + 2
3 c = c + 4
print *, c
if (c /= 7) error stop
end program
