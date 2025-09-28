! Minimal reproducer for legacy array sections segfault
! Matches exact pattern from lapack_06.f90 line 78
PROGRAM TEST
    IMPLICIT NONE
    REAL A(0:100)
    INTEGER M, N, LDA
    REAL ALPHA

    M = 6
    N = 4
    LDA = 7  ! This is M+1 from lapack_06

    ! Initialize array
    A = 1.0

    ! This matches lapack_06.f90 line 78:
    ! CALL STRSM(..., A(1), M+1, ...)
    ! where A is declared as A(0:*)
    CALL STRSM(3, N, ALPHA, A(1), LDA)

    PRINT *, 'Test completed successfully'
END PROGRAM

SUBROUTINE STRSM(M, N, ALPHA, A, LDA)
    IMPLICIT NONE
    INTEGER M, N, LDA
    REAL ALPHA
    REAL A(LDA, *)  ! 2D assumed-size array

    ! Try to access A as a 2D array
    ! In sequence association, A(1,1) should access the first element
    PRINT *, 'Accessing A(1,1) =', A(1,1)
    PRINT *, 'LDA =', LDA
    PRINT *, 'M =', M
    PRINT *, 'N =', N
END SUBROUTINE