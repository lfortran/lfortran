program arrays_57

real :: A(2,2) = reshape([1.0,2.0,3.0,4.0], [2,2])
call trans(A)
print *, A
if ( any(abs( A - 12.91 ) > 1e-6) ) error stop 
call trans_2(A)
print *, A
if ( any(abs( A - 12.91 ) > 1e-6) ) error stop  

contains

subroutine trans(A)
real, intent(inout) :: A(2, 2)
A = matprod(transpose(A))
end subroutine

subroutine trans_2(A)
real, intent(inout) :: A(2, 2)
integer, parameter :: x = 2
A = matprod(abs(A(1:x,:)))
end subroutine

function matprod(x) result(k)
real,intent(in) :: x(:,:)
real :: k(size(x, 1), size(x, 2))
k = 12.91
end function

end program 
