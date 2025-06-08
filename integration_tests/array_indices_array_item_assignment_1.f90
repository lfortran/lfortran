program array_indices_array_item_assignment
integer :: A(3, 2) = reshape([1, 2, 3, 4, 5, 6], [3, 2]), Acorrect(6)
integer :: B(3, 2)
integer :: X(2) = [1,2]
integer, allocatable :: Y(:)
! integer :: i, j, k
allocate(Y(2))
Y = 2
B = 2
A(X, X) = B(X, Y)
Acorrect = reshape(A, [6])
print *, Acorrect
if( any(Acorrect /= [2, 2, 3, 2, 2, 6]) ) error stop
end program
