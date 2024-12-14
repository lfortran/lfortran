program arrays_63
integer :: X(3,3) = reshape([1,2,3,4,5,6,7,8,9], [3,3])
integer, allocatable :: Y(:)
integer :: res(3,3)
allocate(Y(size(X,1)))
Y = [1,2,3]
print *, X(Y,:)
res = X(Y,:)
print *, sum(res)
if (sum(res) /= 45) error stop
end program
