module horner_mod
implicit none
contains

! p = c(1) + c(2) x + ... + c(n) x**(n-1), by Horner's rule
subroutine horner(n, c, x, p)
integer, intent(in) :: n
real(8), intent(in) :: c(n)
real(8), intent(in) :: x
real(8), intent(out) :: p
integer :: i
p = c(n)
do i = 1, n - 1
    p = p*x + c(n - i)
end do
end subroutine

end module
