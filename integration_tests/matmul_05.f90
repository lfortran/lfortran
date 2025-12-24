program matmul_05
    implicit none
    real, allocatable :: A(:,:), B(:,:), C(:,:)
    integer :: i
    
    allocate(A(10,10), B(10,10))
    call random_number(A)
    call random_number(B)
    
    C = matmul(A, B)
    if (.not. allocated(C)) error stop "C should be allocated after first matmul"
    if (any(shape(C) /= [10, 10])) error stop "C should have shape [10, 10]"
    
    do i = 1, 2
        C = matmul(A, B)
    end do
    
    if (.not. allocated(C)) error stop "C should still be allocated"
    if (any(shape(C) /= [10, 10])) error stop "C should have shape [10, 10]"
    
end program matmul_05
