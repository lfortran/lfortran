MODULE module_59_module01
    IMPLICIT NONE
  
    INTEGER, PUBLIC :: nx1 = 1000
    INTEGER, PUBLIC :: nx2 = 1000
  
  END MODULE
  
  MODULE module_59_module02
  
    USE module_59_module01
  
    IMPLICIT NONE
    REAL, ALLOCATABLE, DIMENSION(:), PUBLIC :: ref_flux
  
    REAL, ALLOCATABLE, DIMENSION(:), PUBLIC :: ref_fluxm
  
    CONTAINS
  
    SUBROUTINE mms_deallocate
    END SUBROUTINE mms_deallocate
  
    SUBROUTINE mms_allocate ( )
        nx1 = 555
      ALLOCATE( ref_flux(nx1), ref_fluxm(nx2) )
      print *,"Size of nx1",nx1
      if(size(ref_flux) /= 555) error stop
      print *,"Size of nx2",nx2
      if(size(ref_fluxm) /= 1000) error stop
    END SUBROUTINE mms_allocate
  END MODULE module_59_module02