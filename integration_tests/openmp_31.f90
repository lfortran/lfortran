program openmp_31
    implicit none
    real :: phi(100)
    integer :: j

    phi = 10124.142

    !$omp parallel do private(j) shared(phi)

    do j = 1, 100
        print *, phi(1)
        phi(1) = phi(1) + 1
    end do

    !$omp end parallel do

    print *, phi(1)
    if (abs(phi(1) - 10224.1416) > 1e-8) error stop
end program openmp_31
