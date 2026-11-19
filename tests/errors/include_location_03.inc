subroutine setit(a, n)
implicit none
integer, intent(in) :: n
real(8), intent(out) :: a(2)
integer :: i
do i = 1, n
    a(i) = real(i, 8)
end do
end subroutine
