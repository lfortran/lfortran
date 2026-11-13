! The host still waits for a launch before it reads what the launch wrote when
! a procedure it calls can run a contained procedure that reads it: one whose
! name is handed to another procedure. A contained procedure that only its
! host calls needs no wait before the other calls.
module gpu_deferred_sync_10_mod
implicit none
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

subroutine run_callback(f)
    interface
        subroutine f()
        end subroutine
    end interface
    call f()
end subroutine

subroutine case_callback(r)
    real, intent(out) :: r
    real :: a(64)
    integer :: i
    do concurrent (i = 1:64)
        a(i) = i
    end do
    call scale(a, 2.0)
    call run_callback(show)
    call scale(a, 3.0)
    r = a(2)
contains
    subroutine show()
        if (a(1) /= 2.0) error stop "callback saw a stale a"
    end subroutine
end subroutine

subroutine case_by_name(r)
    real, intent(out) :: r
    real :: a(64)
    integer :: i, n
    n = 0
    do concurrent (i = 1:64)
        a(i) = i
    end do
    call scale(a, 2.0)
    call count_call(n)
    call bump()
    call scale(a, 3.0)
    call count_call(n)
    r = a(2) + n
contains
    subroutine bump()
        n = n + 10
    end subroutine
end subroutine

end module

program gpu_deferred_sync_10
use gpu_deferred_sync_10_mod
implicit none
real :: r
call case_callback(r)
if (abs(r - 12.0) > 1e-5) error stop "callback"
call case_by_name(r)
if (abs(r - 24.0) > 1e-5) error stop "by name"
print *, "ok"
end program
