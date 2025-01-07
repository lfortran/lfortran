program arrays_72
implicit none

real :: xpt(5, 10)

xpt = 2.0
xpt(1, :) = 3.0
xpt(2, :) = 4.0
xpt(3, :) = 5.0
xpt(:, 4) = 6.0
xpt(:, 5) = 7.0
xpt(:, 6) = 8.0
xpt(:, 7) = 9.0

call initq(xpt)

contains

function diag(A) result(D)
implicit none

real(4), intent(in) :: A(:, :)
real(4), allocatable :: D(:)
integer :: dlen, i

dlen = max(0_4, int(min(size(A, 1), size(A, 2)) - 0, 4))
allocate(D(dlen))
D = [(A(i, i), i=1, dlen)]

end function diag

subroutine initq(xpt)
real(4), intent(in), target :: xpt(:, :)
real(4) :: xa(min(size(xpt, 1), size(xpt, 2) - size(xpt, 1) - 1))
real(4) :: xb(size(xa))

integer :: ndiag, n, npt

n = int(size(xpt, 1), kind(n))
npt = int(size(xpt, 2), kind(npt))

ndiag = min(n, npt - n - 1)

xa = diag(xpt(:, 2:ndiag + 1))
xb = diag(xpt(:, n + 2:n + ndiag + 1))

print *, xa
print *, xb
if( any(xa /= [3.0, 4.0, 6.0, 7.0]) ) error stop
if( any(xb /= [9.0, 4.0, 5.0, 2.0]) ) error stop

end subroutine

end program
