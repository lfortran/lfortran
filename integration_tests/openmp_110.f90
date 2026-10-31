program openmp_110
    implicit none
    real :: a(4)
    integer :: i

    do i = 1, 4
        a(i) = real(i)
    end do

    !$omp target enter data map(alloc: a)
    !$omp target update to(a)
    !$omp target update from(a)
    !$omp target exit data map(delete: a)

    do i = 1, 4
        if (a(i) /= real(i)) error stop
    end do
    print *, a
end program
