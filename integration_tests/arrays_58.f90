program arrays_58

real :: A(2,2) = reshape([1.0,2.0,3.0,4.0], [2,2])
call trans(A)
print *, A
if ( any(abs( A - 12.91 ) > 1e-8) ) error stop 
contains

subroutine trans(A)
real, intent(inout) :: A(:, :)
A = matprod(transpose(A))
end subroutine

function matprod(x) result(k)
real,intent(in) :: x(:,:)
real :: k(size(x, 1), size(x, 2))
k = 12.91
end function

end program 
