! The sections in the operand of a reduction that is accumulated in a loop
! are indexed through their base arrays, without a pointer to each section,
! and integer operations are accumulated without a temporary as well.
module array_op_reduction_sections_mod
implicit none
type :: conv
    real, allocatable :: kernel(:, :, :, :)
end type
contains
subroutine forward(self, input, z)
type(conv), intent(in) :: self
real, intent(in) :: input(:, :)
real, intent(out) :: z(:, :)
integer :: n, j
do j = 1, size(z, 2)
    do n = 1, size(z, 1)
        z(n, j) = sum(self%kernel(n, j, :, :) * input(:, j:j + 2))
    end do
end do
end subroutine
subroutine strided(ia, ib, la, m, s)
integer, intent(in) :: ia(:), ib(-2:)
logical, intent(in) :: la(:)
integer, intent(out) :: m
real, intent(out) :: s
m = maxval(ia(1:7:3) * ib(4:-2:-3))
s = sum(real(ia(2:4)), mask=la(1:3))
end subroutine
end module
