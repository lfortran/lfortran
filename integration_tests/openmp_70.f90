! For OMP C Target Constructs Dump
program openmp_70

    implicit none
    real :: a(1000000), b(1000000)
    integer :: i
    b=5
    a=0
    !$omp target map(tofrom:a, b)
        !$omp teams
            !$omp distribute parallel do
                do i = 1, 1000000
                    a(i) = i + b(i)*340
                end do
            !$omp end distribute parallel do
        !$omp end teams
    !$omp end target
    
    print*, a(5), b(5)
    
    if(a(5) /= 1705) error stop
    if(b(5) /= 5) error stop
end program openmp_70