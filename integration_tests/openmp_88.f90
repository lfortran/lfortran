program openmp_88
    ! OpenMP clauses written with blanks before their parenthesis
    implicit none
    integer, parameter :: n = 10
    integer :: i, s, k, a(n)

    a = 0
    !$omp parallel do default (shared) private (i) shared (a) schedule (static) num_threads (2)
    do i = 1, n
        a(i) = i
    end do
    !$omp end parallel do
    do i = 1, n
        if (a(i) /= i) error stop
    end do

    s = 0
    !$omp parallel do private (i) reduction (+:s)
    do i = 1, n
        s = s + a(i)
    end do
    !$omp end parallel do
    if (s /= 55) error stop

    s = 0
    k = 2
    !$OMP PARALLEL DO PRIVATE  (i) FIRSTPRIVATE (k) REDUCTION (+:s)
    do i = 1, n
        s = s + k*a(i)
    end do
    !$OMP END PARALLEL DO
    if (s /= 110) error stop

    print *, s
end program openmp_88
