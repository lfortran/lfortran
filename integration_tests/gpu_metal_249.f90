! A `do concurrent` whose body calls a submodule `module procedure` has
! to splice that callee into the kernel, because the callee builds
! temporaries sized from the extent of an allocatable component of a
! dummy argument and Metal Shading Language has no variable-length
! arrays.
!
! Under separate compilation the callee's body is not part of this
! translation unit: it is loaded from the submodule's `.smod` file.
! Loading builds a fresh copy of the submodule's symbol table each time,
! so resolving the same call twice used to yield two different
! functions.  The offload pass decides which callees to splice in a
! first, non-destructive pass and identifies them by pointer, so the
! splice then never recognised the callee the plan had picked: the loop
! was offloaded but the callee stayed an out-of-line device function
! declaring a variable-length array, and the shader failed to compile.
!
! Both loops below must become kernels -- 2 kernels for this program.
program gpu_metal_249
use gpu_metal_249_mod
implicit none
integer, parameter :: n = 5
type(op_t) :: o
real :: v(2)
real :: out(4, n)
integer :: j

allocate(o%w(2))
o%w = [2.0, 3.0]
v = [1.0, 10.0]

out = -1
do concurrent (j = 1:n)
    out(:, j) = apply(o, v, j)
end do
do j = 1, n
    if (abs(out(1, j) - real(100*j)) > 1.0e-4) error stop "apply lead"
    if (abs(out(2, j) - 2.0) > 1.0e-4) error stop "apply w1"
    if (abs(out(3, j) - 30.0) > 1.0e-4) error stop "apply w2"
    if (abs(out(4, j) - real(100*j)) > 1.0e-4) error stop "apply tail"
end do

out = -1
do concurrent (j = 1:n)
    out(:, j) = twice(o, v, j)
end do
do j = 1, n
    if (abs(out(1, j) - real(10*j)) > 1.0e-4) error stop "twice lead"
    if (abs(out(2, j) - 4.0) > 1.0e-4) error stop "twice w1"
    if (abs(out(3, j) - 60.0) > 1.0e-4) error stop "twice w2"
    if (abs(out(4, j) - real(10*j)) > 1.0e-4) error stop "twice tail"
end do

print *, "ok"
end program
