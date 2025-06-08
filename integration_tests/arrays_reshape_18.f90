program arrays_reshape_18
integer :: A(2,2)
integer :: B(2,2)
A = reshape(source = [1,2,2,1], shape = shape(A))
print *, A(1,1), A(1,2), A(2,1), A(2,2)
if (A(1,1) /= 1) error stop
if (A(1,2) /= 2) error stop
if (A(2,1) /= 2) error stop
if (A(2,2) /= 1) error stop
B = reshape([1,2,2,1], shape = shape(B))
print *, B(1,1), B(1,2), B(2,1), B(2,2)
if (B(1,1) /= 1) error stop
if (B(1,2) /= 2) error stop
if (B(2,1) /= 2) error stop
if (B(2,2) /= 1) error stop
end program
