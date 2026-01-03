MODULE solvar_module
    REAL, DIMENSION(:,:), POINTER :: ptr_out
 CONTAINS
    SUBROUTINE solvar_allocate
       NULLIFY(ptr_out)
       ALLOCATE(ptr_out(5,1))
    END SUBROUTINE solvar_allocate
 
    SUBROUTINE dim3_sweep(ptr_out)
       REAL, DIMENSION(5), INTENT(OUT) :: ptr_out
       PRINT *, ptr_out
       STOP
    END SUBROUTINE dim3_sweep
 
    SUBROUTINE octsweep
       !REAL, DIMENSION(5), pointer :: x
       !x = ptr_out(:,1)
       CALL dim3_sweep( ptr_out(:,1) )
    END SUBROUTINE octsweep
 END MODULE solvar_module
 
 PROGRAM snap_main
    USE solvar_module, ONLY: solvar_allocate, octsweep
    CALL solvar_allocate
    CALL octsweep
 END PROGRAM snap_main