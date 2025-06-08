program arrays_65
integer :: A(2) = [1,2]
integer :: n = 2
integer :: dim = 1
A = sum(spread(A, dim, n), dim=dim)
if (A(1) /= 2) error stop
if (A(2) /= 4) error stop
end program
