! Without Teams/Distribute constructs similar to openmp_71
program openmp_72
    implicit none
    real, allocatable, dimension(:) :: a, b
    integer :: i
    allocate(a(10000000), b(10000000))
    
    b=5
    
    !$omp target map(tofrom:a, b)
        !$omp parallel do
            do i = 1, 10000000
                a(i) = i + b(i)*340
            end do
        !$omp end parallel do
    !$omp end target
    
    print*, a(5), b(5)
    
    if(a(5) /= 1705) error stop
    if(b(5) /= 5) error stop
end program openmp_72