program parameter_08
real, parameter :: A(*) = sin([1.0, 2.0, 3.0, 4.0])
real, parameter :: B(*) = sin(A)
real, parameter :: C(*) = cos(B)
print *, sum(A)
if (abs(sum(A) - 1.13508582) > 1e-8) error stop
print *, sum(B)
if (abs(sum(B) - 0.988748252) > 1e-8) error stop
print *, sum(C)
if (abs(sum(C) - 3.20269895) > 1e-8) error stop
end program
