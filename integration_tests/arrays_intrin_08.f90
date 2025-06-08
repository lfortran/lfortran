program arrays_intrin_08

real :: zmat(4, 4), hdiag(4)
integer :: i, j

do i = 1, 4
    do j = 1, 4
        zmat(i, j) = i + j
    end do
end do

hdiag = calden(zmat)

print *, hdiag
if( any(hdiag /= [28.0, 36.0, 44.0, 52.0]) ) error stop


contains

function calden(zmat) result(hdiag)
implicit none

real(4), intent(in) :: zmat(:, :)  ! ZMAT(NPT, NPT - N - 1)

! Outputs
real(4) :: hdiag(size(zmat, 1))

! Local variables
integer(4) :: idz_loc
idz_loc = 3

hdiag = -sum(zmat(:, 1:idz_loc - 1)**2, dim=2) + sum(zmat(:, idz_loc:size(zmat, 2))**2, dim=2)

end function calden

end program
