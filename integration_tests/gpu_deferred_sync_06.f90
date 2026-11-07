! A kernel whose body can stop, launched by a module subroutine its caller
! calls through the deferring twin: the stop it records is replayed before
! the twin returns, so the program stops before it prints the result.
module gpu_deferred_sync_06_mod
implicit none
contains
subroutine scaled(x, y)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    integer :: i
    do concurrent (i = 1:size(x))
        if (x(i) < 0) error stop "scaled requires nonnegative values"
        y(i) = 2*x(i)
    end do
end subroutine
end module

program gpu_deferred_sync_06
use gpu_deferred_sync_06_mod, only: scaled
implicit none
real :: x(100), y(100)
x = 1
x(37) = -1
call scaled(x, y)
print *, "a negative value was scaled:", sum(y)
end program
