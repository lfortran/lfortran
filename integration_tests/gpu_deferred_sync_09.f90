! The default mode (lfortran/lfortran#17131): the module of a library
! compiled without a device has an empty object, and this unit compiles its
! procedure, which it calls itself and through an external subroutine
! compiled in another file after a kernel. Both calls must reach a defined
! symbol and see what was written before them.
program gpu_deferred_sync_09
use gpu_deferred_sync_09_mod, only: add_one
implicit none
interface
    subroutine gpu_deferred_sync_09_ext(n, a)
    integer, intent(in) :: n
    integer, intent(out) :: a(n)
    end subroutine
end interface
integer, parameter :: n = 1000
integer :: a(n), b(n), i
do i = 1, n
    a(i) = i
end do
call add_one(n, a)
do i = 1, n
    if (a(i) /= i + 1) error stop 1
end do
call gpu_deferred_sync_09_ext(n, b)
do i = 1, n
    if (b(i) /= 2 * i + 1) error stop 2
end do
print *, a(n), b(n)
end program
