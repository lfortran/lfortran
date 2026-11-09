program reductions_hadd_03
! Five points that start from constants with a zero fourth component and
! move under pairwise forces, held as contiguous four element vectors. The
! updates keep the fourth component zero, so with --fast on x86 with AVX
! the horizontal reduction pass sums the squared pair differences without
! clearing it. The result is checked against the same steps on three
! component vectors.
implicit none
integer, parameter :: nb = 5, npair = 10
integer, parameter :: pair_i(npair) = [2,3,3,4,4,4,5,5,5,5]
integer, parameter :: pair_j(npair) = [1,1,2,1,2,3,1,2,3,4]
real(8) :: x(4,nb), v(4,nb), x3(3,nb), v3(3,nb)
integer :: n, i

! The number of steps is only known at run time.
n = 100 + command_argument_count()

call init(x, v)
call orbit(x, v, n)
x3 = 0
v3 = 0
call init3(x3)
call orbit3(x3, v3, n)
do i = 1, nb
    if (any(abs(x(1:3,i) - x3(:,i)) > 1d-10 * (1 + abs(x3(:,i))))) error stop 1
    if (x(4,i) /= 0) error stop 2
end do
print *, "ok"

contains

subroutine init(x, v)
    real(8), intent(out) :: x(4,nb), v(4,nb)
    real(8) :: p(4,nb)
    p(:,1) = [0d0, 0d0, 0d0, 0d0]
    p(:,2) = [1d0, 2d0, 3d0, 0d0]
    p(:,3) = [-2d0, 1d0, 4d0, 0d0]
    p(:,4) = [3d0, -1d0, 2d0, 0d0]
    p(:,5) = [5d0, 3d0, -2d0, 0d0]
    x = p
    v = 0
end subroutine

subroutine init3(x)
    real(8), intent(out) :: x(3,nb)
    x(:,1) = [0d0, 0d0, 0d0]
    x(:,2) = [1d0, 2d0, 3d0]
    x(:,3) = [-2d0, 1d0, 4d0]
    x(:,4) = [3d0, -1d0, 2d0]
    x(:,5) = [5d0, 3d0, -2d0]
end subroutine

subroutine orbit(x, v, n)
    real(8), intent(inout) :: x(4,nb), v(4,nb)
    integer, intent(in) :: n
    real(8) :: r(4,npair), w(npair), d2
    integer :: step, i, k, d
    do step = 1, n
        do k = 1, npair
            do d = 1, 4
                r(d,k) = x(d,pair_i(k)) - x(d,pair_j(k))
            end do
        end do
        do k = 1, npair
            d2 = r(1,k)*r(1,k) + r(2,k)*r(2,k) + r(3,k)*r(3,k)
            w(k) = 0.01d0 / (d2 * sqrt(d2))
        end do
        do k = 1, npair
            do d = 1, 4
                v(d,pair_i(k)) = v(d,pair_i(k)) - r(d,k) * w(k)
                v(d,pair_j(k)) = v(d,pair_j(k)) + r(d,k) * w(k)
            end do
        end do
        do i = 1, nb
            do d = 1, 4
                x(d,i) = x(d,i) + 0.01d0 * v(d,i)
            end do
        end do
    end do
end subroutine

subroutine orbit3(x, v, n)
    real(8), intent(inout) :: x(3,nb), v(3,nb)
    integer, intent(in) :: n
    real(8) :: r(3), w
    integer :: step, i, j, k
    do step = 1, n
        do k = 1, npair
            i = pair_i(k)
            j = pair_j(k)
            r = x(:,i) - x(:,j)
            w = 0.01d0 / (sum(r**2) * sqrt(sum(r**2)))
            v(:,i) = v(:,i) - r * w
            v(:,j) = v(:,j) + r * w
        end do
        x = x + 0.01d0 * v
    end do
end subroutine

end program reductions_hadd_03
