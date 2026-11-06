program gpu_wait_reads_01
! The host changing or freeing an array a pending launch only reads, which a
! runtime may hand the kernel in place: after a loop of launches, through a
! procedure, by deallocating it, and as a local of a `block` that ends. The
! arrays are larger than the size up to which a runtime that uses host
! memory in place still copies an array a kernel only reads.
implicit none
integer, parameter :: dp = kind(0.d0)
integer, parameter :: n = 2**21
real(dp), allocatable :: x(:), y(:), z(:)
integer :: i, it

allocate(x(n), y(n), z(n))
do i = 1, n
    x(i) = i
    y(i) = 0
end do

! Launches that share what they read, then a change of it.
do it = 1, 4
    do concurrent (i = 1:n)
        y(i) = y(i) + slow(x(i))
    end do
end do
x = -1
if (abs(y(n) - 4*slow(real(n, dp))) > 1e-6_dp*abs(y(n))) error stop 1
if (abs(y(1) - 4*slow(1.0_dp)) > 1e-12_dp) error stop 2

! A change by a procedure.
do i = 1, n
    x(i) = i
end do
do concurrent (i = 1:n)
    z(i) = slow(x(i))
end do
call fill(x, -2.0_dp)
if (abs(z(n) - slow(real(n, dp))) > 1e-6_dp*abs(z(n))) error stop 3

! Deallocating it.
do i = 1, n
    x(i) = 3*i
end do
do concurrent (i = 1:n)
    z(i) = slow(x(i))
end do
deallocate(x)
if (abs(z(n) - slow(3*real(n, dp))) > 1e-6_dp*abs(z(n))) error stop 4

! A local of a block.
block
    real(dp), allocatable :: w(:)
    allocate(w(n))
    do i = 1, n
        w(i) = 5*i
    end do
    do concurrent (i = 1:n)
        z(i) = slow(w(i))
    end do
end block
allocate(x(n))
x = -3
if (abs(z(n) - slow(5*real(n, dp))) > 1e-6_dp*abs(z(n))) error stop 5
if (abs(z(1) - slow(5.0_dp)) > 1e-12_dp) error stop 6
print *, y(n), z(n)

contains

    ! Some work before the value is used, so that the kernel runs for a
    ! while after its launch returns.
    pure real(dp) function slow(v) result(r)
        real(dp), intent(in) :: v
        integer :: k
        r = v
        do k = 1, 50
            r = 0.5_dp*(r + v)
        end do
    end function

    subroutine fill(a, v)
        real(dp), intent(out) :: a(:)
        real(dp), intent(in) :: v
        a = v
    end subroutine

end program
