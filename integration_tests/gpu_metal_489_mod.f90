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
        f(k) = pair(q, i, j0 + k - 2)
    end do
    do k = 1, rows
        r(k) = f(k + 1) - f(k) + left(q, i, j0, k)
    end do
contains

    ! A contained procedure with a named constant of its own.
    pure real function pair(q, i, j)
        real, intent(in) :: q(:,:)
        integer, intent(in) :: i, j
        integer, parameter :: two = 2
        real :: t(two)
        integer :: m
        do m = 1, two
            t(m) = q(i, j + m - 1)
        end do
        pair = t(1) + t(2)
    end function pair

    ! A contained procedure using named constants of the module.
    pure real function left(q, i, j0, k)
        real, intent(in) :: q(:,:)
        integer, intent(in) :: i, j0, k
        real :: c(rows)
        integer :: m
        do m = 1, rows
            c(m) = q(i - 1, j0 + m - 1)
        end do
        left = c(k)
    end function left

end subroutine block_sums

end module gpu_metal_489_m
