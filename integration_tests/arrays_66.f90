program arrays_66

real :: Q(8, 8)
integer :: i, j

do i = 1, 8
    do j = 1, 8
        Q(i, j) = i*j + i + j + 1
    end do
end do

call qradd_Rdiag(Q, 1)

print *, Q(:, 1)
if( any(Q(:, 1) /= [4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0]) ) error stop

do j = 2, 8
    if( any(Q(:, j) /= 1.0) ) error stop
end do

contains

function matprod(Q, G) result(R)
real, intent(in) :: Q(:, :), G(:, :)
real :: R(size(Q, 1), size(G, 2))
R = 1.0
end function

subroutine qradd_Rdiag(Q, n)  ! Used in COBYLA

! In-outputs
integer(4), intent(in) :: n
real(4), intent(inout) :: Q(:, :)  ! Q(M, M)

integer(4) :: k
integer(4) :: m
real(4) :: G(2, 2)

! Sizes
m = int(size(Q, 2), kind(m))

do k = m - 1_4, n + 1_4, -1
    Q(:, [k, k + 1_4]) = matprod(Q(:, [k, k + 1_4]), transpose(G))
end do

end subroutine qradd_Rdiag

end program
