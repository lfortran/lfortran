! OpenMP directives continued onto following `!$omp` lines with `&`
subroutine continued_directive(a, n)
    implicit none
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    integer :: i, j
    !$omp parallel do schedule(static) &
    !$omp   private(i, j)
    do i = 1, n
        j = i
        a(j) = a(j)*2.0
    end do
    !$omp end parallel do
end subroutine continued_directive

program openmp_100
    use omp_lib
    implicit none
    integer, parameter :: n = 100
    real :: a(n)
    integer :: i, j, k, s, t, nthreads
    a = 1.0
    call continued_directive(a, n)
    do i = 1, n
        if (abs(a(i) - 2.0) > 1e-5) error stop
    end do

    ! Several continuation lines, with and without a leading `&`
    s = 0
    !$omp parallel do &
    !$omp& private(j) &
    !$omp&   reduction(+:s)
    do i = 1, n
        j = int(a(i))
        s = s + j
    end do
    !$omp end parallel do
    if (s /= 2*n) error stop

    ! Continuation inside a clause list, upper case sentinel
    t = 0
    !$OMP PARALLEL DO PRIVATE(j, &
    !$OMP k) REDUCTION(+:t)
    do i = 1, n
        j = i
        k = 2*j
        t = t + k - j
    end do
    !$OMP END PARALLEL DO
    if (t /= n*(n+1)/2) error stop

    ! A clause split across lines in the middle of a token
    nthreads = 0
    !$omp parallel num_thr&
    !$omp&eads(2) shared(nthreads)
    !$omp single
    nthreads = omp_get_num_threads()
    !$omp end single
    !$omp end &
    !$omp parallel
    if (nthreads /= 2) error stop
    print *, s, t, nthreads
end program openmp_100
