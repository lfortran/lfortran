program openmp_77
! An `!$omp target` region with no device to offload it to. Without
! `--openmp` the sentinel is a comment and the loop runs on one thread; with
! it the iterations are spread over the threads of the host instead.
implicit none
integer, parameter :: n = 1000
real :: a(n), b(n)
integer :: i

do i = 1, n
    a(i) = real(i)
end do

!$omp target map(to: a) map(from: b)
    !$omp teams
        !$omp distribute parallel do
            do i = 1, n
                b(i) = 2.0 * a(i) + 1.0
            end do
        !$omp end distribute parallel do
    !$omp end teams
!$omp end target

do i = 1, n
    if (abs(b(i) - (2.0 * real(i) + 1.0)) > 1e-3) error stop
end do
print *, "ok"
end program
