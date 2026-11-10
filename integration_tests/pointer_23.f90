! Pointer assignment of NULL() to rank-6 module array pointers (#4674)
MODULE pointer_23_mod
    IMPLICIT NONE
    REAL, POINTER :: ptr_in(:,:,:,:,:,:), ptr_out(:,:,:,:,:,:)
CONTAINS

    SUBROUTINE solvar_allocate()
        IMPLICIT NONE
        INTEGER :: ierr

        ptr_in => NULL()
        ptr_out => NULL()
        IF (ASSOCIATED(ptr_in)) ERROR STOP
        IF (ASSOCIATED(ptr_out)) ERROR STOP

        ALLOCATE(ptr_in(2,2,2,2,2,2), STAT=ierr)
        IF (ierr /= 0) THEN
            PRINT *, "Error in allocation"
            ERROR STOP
        END IF
        ptr_in = 1.5

        ptr_out => ptr_in

        PRINT *, "ptr_in associated: ", ASSOCIATED(ptr_in)
        PRINT *, "ptr_out associated: ", ASSOCIATED(ptr_out)
        IF (.NOT. ASSOCIATED(ptr_in)) ERROR STOP
        IF (.NOT. ASSOCIATED(ptr_out, ptr_in)) ERROR STOP
        IF (SIZE(ptr_out) /= 64) ERROR STOP
        IF (ABS(SUM(ptr_out) - 96.0) > 1e-5) ERROR STOP

        DEALLOCATE(ptr_in)
        ptr_in => NULL()
        ptr_out => NULL()

        PRINT *, "ptr_in nullified: ", .NOT. ASSOCIATED(ptr_in)
        PRINT *, "ptr_out nullified: ", .NOT. ASSOCIATED(ptr_out)
        IF (ASSOCIATED(ptr_in)) ERROR STOP
        IF (ASSOCIATED(ptr_out)) ERROR STOP
    END SUBROUTINE solvar_allocate

END MODULE pointer_23_mod

PROGRAM pointer_23
    USE pointer_23_mod
    IMPLICIT NONE
    CALL solvar_allocate()
END PROGRAM pointer_23
