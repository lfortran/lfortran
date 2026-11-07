! A file compiled with a device calls the procedures of a module of a
! library compiled without one (lfortran/lfortran#17131): the calls reach
! the procedures themselves, as the library defines no deferring twins, and
! see what the kernels of this file wrote.
program gpu_deferred_sync_08
use gpu_deferred_sync_08_mod, only: scale, fill, total
implicit none
integer, parameter :: n = 1000
real :: a(n), t
integer :: b(n), i
do concurrent (i = 1:n)
    a(i) = real(i)
end do
call scale(a, 2.0)
do i = 1, n
    if (a(i) /= 2.0 * real(i)) error stop 1
end do
call fill(n, b)
do concurrent (i = 1:n)
    b(i) = b(i) + 1
end do
do i = 1, n
    if (b(i) /= 3 * i + 1) error stop 2
end do
do concurrent (i = 1:n)
    a(i) = 1.0
end do
call total(a, t)
if (t /= real(n)) error stop 3
print *, a(1), b(n), t
end program
