! For CUDA C Dump of OpenMP Target Constructs
program openmp_71
    implicit none
    real, allocatable, dimension(:) :: a, b
    integer :: i
    allocate(a(10000000), b(10000000))
    
    b=5
    
    !$omp target map(tofrom:a, b)
        !$omp teams
            !$omp distribute parallel do
                do i = 1, 10000000
                    a(i) = i + b(i)*340
                end do
            !$omp end distribute parallel do
        !$omp end teams
    !$omp end target
    
    print*, a(5), b(5)
    
    if(a(5) /= 1705) error stop
    if(b(5) /= 5) error stop
end program openmp_71