program stack_array_automatic_elements
! With --stack-arrays, automatic arrays are allocated on the stack. Each
! element must get its full size, so that writing one array does not
! overwrite its neighbour, also for elements wider than a pointer.
implicit none
real(8) :: s
call work(1000, s)
print *, s
if (abs(s - 1001000.0d0) > 1d-6) error stop 1
contains
subroutine work(n, s)
    integer, intent(in) :: n
    real(8), intent(out) :: s
    complex(8) :: z(n)
    real(8) :: guard(n)
    real :: w(n, 2)
    integer :: i
    do i = 1, n
        guard(i) = i
    end do
    do i = 1, n
        z(i) = cmplx(i, -i, 8)
        w(i, :) = 0.5
    end do
    if (abs(sum(aimag(z)) + 500500.0d0) > 1d-6) error stop 2
    if (abs(sum(w) - n) > 1e-3) error stop 3
    s = sum(guard) + sum(real(z))
end subroutine
end program
