module legendre_mod
implicit none
integer, parameter :: dp = kind(1.d0)

contains

! Legendre polynomial P_n(x) via the three-term recurrence
!   P_0 = 1, P_1 = x, k P_k = (2k-1) x P_{k-1} - (k-1) P_{k-2}
function legendre_poly(n, x) result(p)
    integer, intent(in) :: n
    real(dp), intent(in) :: x
    real(dp) :: p
    real(dp) :: a, b, c
    integer :: k
    a = 1.0_dp
    b = x

    if (n == 0) then
        p = a
        return
    end if

    do k = 2, n
        c = ((2*k-1)*x*b - (k-1)*a)/k
        a = b
        b = c
    end do

    p = b
end function

end module
