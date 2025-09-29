! Minimal reproducer for legacy array sections with assumed-size arrays
! Tests sequence association pattern: 0-based array passing element to 2D assumed-size formal
! This is a known edge case that requires caller/callee calling convention agreement
PROGRAM TEST
    IMPLICIT NONE
    REAL A(0:100)
    INTEGER M, N, LDA
    REAL ALPHA

    M = 6
    N = 4
    LDA = 7  ! This is M+1 from LAPACK pattern

    ! Initialize array
    A = 1.0

    ! Pattern: pass A(1) from 0-based array to assumed-size 2D formal
    ! This tests whether legacy array sections handles:
    ! - 0-based array indexing
    ! - Element-to-array sequence association
    ! - Assumed-size formal parameter (*)
    CALL STRSM(M, N, ALPHA, A(1), LDA)

    PRINT *, 'Test completed successfully'
END PROGRAM

SUBROUTINE STRSM(M, N, ALPHA, A, LDA)
    IMPLICIT NONE
    INTEGER M, N, LDA
    REAL ALPHA
    REAL A(LDA, *)  ! 2D assumed-size array

    ! Access A as 2D array - tests descriptor/pointer handling
    PRINT *, 'A(1,1) =', A(1,1)
    PRINT *, 'A(2,1) =', A(2,1)
    IF (A(1,1) /= 1.0) ERROR STOP 'A(1,1) should be 1.0'
    IF (A(2,1) /= 1.0) ERROR STOP 'A(2,1) should be 1.0'
END SUBROUTINE