program test 
integer :: A(3,2) = reshape([1,2,3,4,5,6], [3,2])
integer :: X(2)
X = abs(sum(A, dim=1))
if(X(1) /= 6) error stop 
if(X(2) /= 15) error stop
end program