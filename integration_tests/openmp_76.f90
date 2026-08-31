program openmp_76
! An `!$omp parallel do` loop offloaded onto a device. The construct asks for
! the threads of the host, so the compiler only puts it on a device when it
! is asked to with `--gpu-offload-omp-loops`.
implicit none
integer, parameter :: n = 1000
real :: a(n), b(n)
integer :: i

do i = 1, n
    a(i) = real(i)
end do

!$omp parallel do private(i)
do i = 1, n
    b(i) = 2.0 * a(i) + 1.0
end do
!$omp end parallel do

do i = 1, n
    if (abs(b(i) - (2.0 * real(i) + 1.0)) > 1e-3) error stop
end do
print *, "ok"
end program
