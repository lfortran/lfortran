program lapack_testing_03
    implicit none

    integer :: m, n, nrhs, ldwork, lwork
    real, allocatable :: work(:)
    real :: result

    m = 2
    n = 1
    nrhs = 3
    ldwork = m
    lwork = m*nrhs + m + 10

    allocate(work(lwork))
    work = 0.0

    call sqrt16(m, n, nrhs, work, ldwork, work(m*nrhs+1), result)

    if (abs(result - 2.0) > 1e-6) error stop
end program lapack_testing_03
