MODULE module_58_module01
    IMPLICIT NONE
  
    INTEGER, PUBLIC :: nx = 1000
  
  END MODULE
  
  MODULE module_58_module02
  
    USE module_58_module01, ONLY: nx
  
    IMPLICIT NONE
    REAL, ALLOCATABLE, DIMENSION(:), PUBLIC :: ref_flux
  
    REAL, ALLOCATABLE, DIMENSION(:), PUBLIC :: ref_fluxm
  
    CONTAINS
  
    SUBROUTINE mms_deallocate
    END SUBROUTINE mms_deallocate
  
    SUBROUTINE mms_allocate ( )
      ALLOCATE( ref_flux(nx), ref_fluxm(nx) )
    END SUBROUTINE mms_allocate
  END MODULE module_58_module02