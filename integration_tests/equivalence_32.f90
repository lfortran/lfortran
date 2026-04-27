program equivalence_32
! Test WRITE to character array that is in an EQUIVALENCE statement
implicit none
character(5) :: a(2)
character(1) :: b(10)
equivalence (a, b)

write(unit=a, fmt='(I5)') 42
if (a(1) /= "   42") error stop
print *, a(1)
end program
