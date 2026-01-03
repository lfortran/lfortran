program openmp_32
    use omp_lib
    implicit none
    real :: phi
    integer :: j, i

    phi = 10124.142

    call omp_set_num_threads(4)
    do i = 1, 10
        !$omp parallel do private(j) reduction(+:phi)
        do j = 1, 100
            phi = phi + 1.0
        end do
        !$omp end parallel do
    end do

    print *, phi
    if (abs(phi - 11124.1416) > 1e-8) error stop
end program openmp_32
