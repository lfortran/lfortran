! Calls, from a file compiled with a GPU device, the module subroutines of
! a host library compiled without one: the program links (it calls no
! deferring twin the library does not define), and each subroutine sees
! what the kernels before its call wrote.
program gpu_deferred_sync_08
use gpu_deferred_sync_08_mod, only: extent_create, extent_dims, total
implicit none
integer :: dims(2), got(2), maxdims(2), id, i
real :: x(1000), s
do concurrent (i = 1:2)
    dims(i) = 3*i
end do
call extent_create(2, dims, id)
if (id /= 36) error stop
call extent_dims(id, got, maxdims)
if (any(got /= [3, 6])) error stop
if (any(maxdims /= -1)) error stop
do concurrent (i = 1:size(x))
    x(i) = 2
end do
call total(x, s)
if (abs(s - 2000) > 1e-3) error stop
print *, id, got, s
end program
