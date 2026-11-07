program reductions_hadd_02
! Sums of squares of the first three components of the ten pair products
! of five points held as contiguous four element vectors. With --fast on x86
! with AVX the horizontal reduction pass rewrites these sums as horizontal
! adds, which also add the fourth component. The fourth component of a
! product is zero only if it is zero in a factor; in the second call it is
! nonzero in both, so the pass must clear it before the horizontal adds.
implicit none
integer, parameter :: np = 5, npair = np*(np-1)/2
integer, parameter :: pair_i(npair) = [2,3,3,4,4,4,5,5,5,5]
integer, parameter :: pair_j(npair) = [1,1,2,1,2,3,1,2,3,4]
real(8) :: x(4,np), d2(npair), s
integer :: i

! The scale is only known at run time, so nothing below is constant folded.
s = 1 + command_argument_count()

do i = 1, np
    x(:,i) = [s*i, s*i*i, s*(np-i), 0.0d0]
end do
call products(x, d2)
call check(x, d2)

! A nonzero fourth component must not contribute to the sum.
x(4,:) = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
call products(x, d2)
call check(x, d2)

print *, "ok"

contains

subroutine products(x, d2)
    real(8), intent(in) :: x(4,np)
    real(8), intent(out) :: d2(npair)
    real(8) :: r(4,npair)
    integer :: k, d
    do k = 1, npair
        do d = 1, 4
            r(d,k) = x(d,pair_i(k)) * x(d,pair_j(k))
        end do
    end do
    do k = 1, npair
        d2(k) = r(1,k)*r(1,k) + r(2,k)*r(2,k) + r(3,k)*r(3,k)
    end do
end subroutine

subroutine check(x, d2)
    real(8), intent(in) :: x(4,np), d2(npair)
    real(8) :: expected
    integer :: k
    do k = 1, npair
        expected = sum((x(1:3,pair_i(k)) * x(1:3,pair_j(k)))**2)
        if (abs(d2(k) - expected) > 1d-12 * expected) error stop 1
    end do
end subroutine

end program reductions_hadd_02
