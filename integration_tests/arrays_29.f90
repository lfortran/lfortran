program arrays_29
real :: B(10, 2), rescol1, rescol2

B(:, 1) = 1
B(:, 2) = 2

rescol1 = mysum(size(B, 1), B(:, 1))
print *, rescol1
if( rescol1 /= 10.0 ) error stop

rescol2 = mysum(size(B, 1), B(:, 2))
print *, rescol2
if( mysum(size(B, 1), B(:, 2)) /= 20.0 ) error stop

contains

    real function mysum(n, A) result(r)
    integer, intent(in) :: n
    real, intent(in) :: A(n)
    integer :: i
    r = 0
    do i = 1, size(A)
        r = r + A(i)
    end do
    end function

end program
