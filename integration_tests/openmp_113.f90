program openmp_113
    use omp_lib
    implicit none
    integer, parameter :: n = 8, m = 100000
    integer :: i, j
    real(8) :: u, s(n), lo(n), hi(n)
    call omp_set_num_threads(4)
    s = 0d0
    lo = 1d0
    hi = 0d0
    !$omp parallel private(i,j,u)
    !$omp do
    do i = 1, n
        do j = 1, m
            call random_number(u)
            s(i) = s(i) + u
            lo(i) = min(lo(i), u)
            hi(i) = max(hi(i), u)
        end do
    end do
    !$omp end do
    !$omp end parallel
    do i = 1, n
        if (lo(i) < 0d0 .or. hi(i) >= 1d0) error stop "value outside [0,1)"
        if (abs(s(i) / m - 0.5d0) > 0.02d0) error stop "mean is not close to 0.5"
    end do
    print *, "ok"
end program
