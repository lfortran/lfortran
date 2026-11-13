! The host still waits for a launch before it reads what the launch wrote when
! a procedure it calls can reach that storage through a pointer: a target that
! a module pointer is associated with.
module gpu_deferred_sync_11_mod
implicit none
real, pointer :: shared(:) => null()
integer :: calls = 0
contains
subroutine scale(x, s)
    real, intent(inout) :: x(:)
    real, intent(in) :: s
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = s*x(i)
    end do
end subroutine

subroutine count_call(n)
    integer, intent(inout) :: n
    n = n + 1
end subroutine

subroutine publish(x)
    real, target, intent(in) :: x(:)
    shared => x
end subroutine

real function read_shared()
    read_shared = shared(1)
end function

subroutine case_pointer(r)
    real, intent(out) :: r
    real, target :: a(64)
    integer :: i
    do concurrent (i = 1:64)
        a(i) = i
    end do
    call scale(a, 2.0)
    call publish(a)
    call scale(a, 3.0)
    if (read_shared() /= 6.0) error stop "shared saw a stale a"
    r = a(2)
    shared => null()
end subroutine
end module

program gpu_deferred_sync_11
use gpu_deferred_sync_11_mod
implicit none
real :: r
call case_pointer(r)
if (abs(r - 12.0) > 1e-5) error stop "pointer"
print *, "ok"
end program
