! A rank two array of a derived type whose extents are runtime expressions.
! The flattened component buffers are laid out in element order, so every
! subscript after the first is counted by the extent of the dimensions
! before it; that stride has to be the real extent rather than a guess, or
! the kernel reads a different element of the array altogether.
module gpu_metal_322_mod
implicit none
type :: cell_t
    real :: a
    real, allocatable :: v(:)
end type
contains
subroutine work(n, m, cells, r)
    integer, intent(in) :: n, m
    type(cell_t), intent(in) :: cells(2*n, m)
    real, intent(out) :: r(4)
    integer :: i
    do concurrent (i = 1:4)
        r(i) = cells(i, 3)%a + cells(2, i)%v(1)
    end do
end subroutine
end module

program gpu_metal_322
use gpu_metal_322_mod
implicit none
type(cell_t) :: cells(4,4)
real :: r(4)
integer :: i, j

do j = 1, 4
    do i = 1, 4
        cells(i,j)%a = real(i) + 10.0*real(j)
        allocate(cells(i,j)%v(2))
        cells(i,j)%v = [real(i)*100.0 + real(j), 0.0]
    end do
end do

call work(2, 4, cells, r)

do i = 1, 4
    if (abs(r(i) - (2.0*real(i) + 230.0)) > 1.0e-5) error stop
end do

do j = 1, 4
    do i = 1, 4
        deallocate(cells(i,j)%v)
    end do
end do

print *, "PASS"
end program
