module gpu_metal_489_m
implicit none
private
public :: block_sums

! Named constants that only the module's procedures refer to.
integer, parameter :: rows = 3
integer, parameter :: span = rows + 1

contains

! The sums of the pairs of consecutive values q(i, j0 - 1:j0 + rows - 1),
! less the value to their left, for the rows cells (i, j0:j0 + rows - 1).
pure subroutine block_sums(q, i, j0, r)
    real, intent(in) :: q(:,:)
    integer, intent(in) :: i, j0
    real, intent(out) :: r(rows)
    real :: f(span)
    integer :: k
    do k = 1, span
        f(k) = q(i, j0 + k - 2) + q(i, j0 + k - 1)
    end do
    do k = 1, rows
        r(k) = f(k + 1) - f(k) + q(i - 1, j0 + k - 1)
    end do
end subroutine block_sums

end module gpu_metal_489_m
