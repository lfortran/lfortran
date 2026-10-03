! A collapsed loop nest of an omp target region
program openmp_75
    implicit none
    integer, parameter :: n = 64, m = 32
    real :: a(n, m)
    integer :: i, j

    a = 0

    !$omp target map(tofrom:a)
        !$omp teams
            !$omp distribute parallel do collapse(2)
                do j = 1, m
                    do i = 1, n
                        a(i, j) = real(i) * 100 + real(j)
                    end do
                end do
            !$omp end distribute parallel do
        !$omp end teams
    !$omp end target

    do j = 1, m
        do i = 1, n
            if (abs(a(i, j) - (real(i) * 100 + real(j))) > 1.0e-5) error stop
        end do
    end do

    print *, a(5, 7)
end program openmp_75
