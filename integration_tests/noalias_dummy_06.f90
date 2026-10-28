! A VOLATILE dummy argument can be changed by means the procedure does not
! see, so its parameter is not noalias under --fast: the loop must read x(1)
! again after each call.
module noalias_dummy_06_m
implicit none
interface
    subroutine noalias_dummy_06_complete(k)
        integer, intent(in) :: k
    end subroutine
end interface
contains

subroutine consume(x, n, total)
    integer, intent(in) :: n
    real, volatile :: x(n)
    real, intent(out) :: total
    integer :: k
    total = 0
    do k = 1, n
        call noalias_dummy_06_complete(k)
        total = total + x(1)
    end do
end subroutine

subroutine consume_scoped(x, n, total)
    integer, intent(in) :: n
    real :: x(n)
    real, intent(out) :: total
    integer :: k
    block
        volatile :: x
        total = 0
        do k = 1, n
            call noalias_dummy_06_complete(k)
            total = total + x(1)
        end do
    end block
end subroutine

end module

program noalias_dummy_06
use iso_c_binding, only: c_loc
use noalias_dummy_06_pending, only: pending_buffer
use noalias_dummy_06_m, only: consume, consume_scoped
implicit none
real, target, volatile :: buf(4)
real :: total
buf = 0
pending_buffer = c_loc(buf)
call consume(buf, 4, total)
print *, total
if (abs(total - 10) > 1e-6) error stop
buf = 0
call consume_scoped(buf, 4, total)
if (abs(total - 10) > 1e-6) error stop
end program
