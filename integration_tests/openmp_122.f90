! Use-associated module variables inside `do concurrent` (#18076, #18099).
module openmp_119_m
implicit none
real(8), allocatable :: f(:)
integer :: n
integer :: c
end module

subroutine openmp_119_assign()
use openmp_119_m
implicit none
integer :: i
do concurrent (i = 1:n)
    f(i) = 1.0d0
end do
end subroutine

subroutine openmp_119_read()
use openmp_119_m, only: c, n
implicit none
integer :: i, a(4)
do concurrent (i = 1:n)
    a(i) = c
end do
do i = 1, n
    if (a(i) /= 7) error stop
end do
end subroutine

program openmp_119
use openmp_119_m
implicit none
integer :: i
n = 4
c = 7
allocate(f(n))
f = 0.0d0
call openmp_119_assign()
do i = 1, n
    if (f(i) /= 1.0d0) error stop
end do
call openmp_119_read()
print *, "ok"
end program
