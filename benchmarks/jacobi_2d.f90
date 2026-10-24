! Jacobi iterations of the 5-point Laplace stencil on a square grid.
program jacobi_2d
implicit none
integer, parameter :: dp = kind(0.d0)
integer, parameter :: n = 1024, niter = 1000
real(dp), allocatable :: u(:,:), v(:,:)
real(dp) :: s
integer :: i, j, it

allocate(u(0:n+1,0:n+1), v(0:n+1,0:n+1))
u = 0
u(0,:) = 1
v = u

do it = 1, niter
    do j = 1, n
        do i = 1, n
            v(i,j) = 0.25_dp * (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))
        end do
    end do
    do j = 1, n
        do i = 1, n
            u(i,j) = 0.25_dp * (v(i-1,j) + v(i+1,j) + v(i,j-1) + v(i,j+1))
        end do
    end do
end do

s = sum(u(1:n,1:n))
print *, "checksum:", s
! The solution stays between the boundary values 0 and 1
if (minval(u(1:n,1:n)) < 0 .or. maxval(u(1:n,1:n)) > 1) error stop
if (s <= 0) error stop
end program
