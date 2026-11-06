! With --fast (#16665), a dummy procedure that is called in an OpenMP
! parallel region is not paired (the region is outlined before the pass
! runs), so the internal procedure passed to it, also from a BLOCK, still
! gets a thunk: a recursion less deep than --internal-procedure-thunks.
module frame_specialization_07_m
implicit none
contains
    recursive integer function a(n, x) result(r)
        integer, intent(in) :: n
        procedure(base) :: x
        integer :: t, i
        if (n == 0) then
            r = x(1)
            return
        end if
        block
            integer :: q
            q = n
            r = a(n - 1, b) + q
        end block
        t = 0
        !$omp parallel do reduction(+:t)
        do i = 1, 4
            t = t + x(i)
        end do
        !$omp end parallel do
        r = r + t
    contains
        integer function b(i)
            integer, intent(in) :: i
            b = x(i) + n
        end function
    end function
    integer function base(i)
        integer, intent(in) :: i
        base = i
    end function
end module
program frame_specialization_07
use frame_specialization_07_m
implicit none
integer :: r
r = a(6, base)
print *, r
if (r /= 383) error stop 1
end program
