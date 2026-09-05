! An omp target region whose nest also says how many teams and threads to
! run it with. A device chooses its own layout and reports that it ignored
! the clauses; the host threads honour them. Either way the loop runs.
program openmp_79
    implicit none
    integer, parameter :: n = 1000
    real :: a(n), b(n)
    integer :: i

    a = 3.0
    b = 0.0

    !$omp target map(to:a) map(from:b)
        !$omp teams num_teams(4) thread_limit(64)
            !$omp distribute parallel do num_threads(8)
                do i = 1, n
                    b(i) = 2.0 * a(i) + 1.0
                end do
            !$omp end distribute parallel do
        !$omp end teams
    !$omp end target

    do i = 1, n
        if (abs(b(i) - 7.0) > 1.0e-5) error stop
    end do

    print *, b(7)
end program openmp_79
