program openmp_40
    use omp_lib
    integer :: i
    do i = 1, 5
        print *, i
    end do 
end program