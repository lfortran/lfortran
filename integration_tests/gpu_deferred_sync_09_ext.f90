! An external subroutine (a file without a main program) that calls the
! procedure of a module of a library compiled without a device, after a
! kernel wrote its argument. The unit with the main program defines it.
subroutine gpu_deferred_sync_09_ext(n, a)
use gpu_deferred_sync_09_mod, only: add_one
implicit none
integer, intent(in) :: n
integer, intent(out) :: a(n)
integer :: i
do concurrent (i = 1:n)
    a(i) = 2 * i
end do
call add_one(n, a)
end subroutine
