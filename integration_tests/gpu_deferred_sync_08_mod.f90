! A library compiled for the host only (HOST_STATIC_LIBRARY): its object
! defines these subroutines and none of the deferring twins a file compiled
! with a device gives them.
module gpu_deferred_sync_08_mod
implicit none
contains
subroutine scale(a, s)
    real, intent(inout) :: a(:)
    real, intent(in) :: s
    a = a * s
end subroutine

subroutine fill(n, b)
    integer, intent(in) :: n
    integer, intent(out) :: b(n)
    integer :: i
    do i = 1, n
        b(i) = 3 * i
    end do
end subroutine

subroutine total(a, t)
    real, intent(in) :: a(:)
    real, intent(out) :: t
    t = sum(a)
end subroutine
end module
