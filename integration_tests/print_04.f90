program print_04
implicit none
integer :: x
character(len=5) :: f = "(5i3)"
x = (2+3)*5
print "(5i3)", x, 1, 3, x, (2+3)*5+x
write(*,"(5i3)") x, 1, 3, x, (2+3)*5+x

print f, x, 1, 3, x, (2+3)*5+x
write(*,f) x, 1, 3, x, (2+3)*5+x

print 1000, x, 1, 3, x, (2+3)*5+x
write(*,1000) x, 1, 3, x, (2+3)*5+x

1000 format(5i3)
end program
