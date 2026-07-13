program write_39
implicit none

character(len=2) :: c(5)

c = "?"
write(unit=c(5:1:-2), fmt="(A)") "x", "y", "z"

if (c(1) /= "z" .or. c(2) /= "?" .or. c(3) /= "y" .or. &
    c(4) /= "?" .or. c(5) /= "x") error stop
print *, "Test passed"
end program
