program array_indices_array_item_assignment 
integer :: A(3) = [1,2,3]
integer :: X(2) = [1,2]
integer :: Y = 2
A(X) = Y
print * , A
end program