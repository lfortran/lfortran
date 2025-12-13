! MRE: LAPACK-style 2D assumed-size array with leading dimension
! Tests: PointerArray ABI for assumed-size dummy arguments
! Pattern: subroutine with A(LDA, *) passed from caller
program lapack_05
    implicit none
    integer, parameter :: LDA = 4, N = 3
    real :: A(LDA, N), B(LDA, N)
    integer :: i, j
    
    ! Initialize arrays
    do j = 1, N
        do i = 1, LDA
            A(i,j) = real(i + (j-1)*LDA)
            B(i,j) = 0.0
        end do
    end do
    
    ! Call LAPACK-style subroutine
    call copy_matrix(LDA, N, A, LDA, B, LDA)
    
    ! Verify result
    do j = 1, N
        do i = 1, LDA
            if (abs(B(i,j) - A(i,j)) > 1.0e-6) error stop 1
        end do
    end do
    
    print *, "PASS"
end program

! LAPACK-style subroutine with assumed-size arrays
subroutine copy_matrix(M, N, A, LDA, B, LDB)
    implicit none
    integer, intent(in) :: M, N, LDA, LDB
    real, intent(in) :: A(LDA, *)
    real, intent(out) :: B(LDB, *)
    integer :: i, j
    
    do j = 1, N
        do i = 1, M
            B(i,j) = A(i,j)
        end do
    end do
end subroutine
