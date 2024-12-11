PROGRAM arrays_04_size

    IMPLICIT NONE
    
    INTEGER(2) :: nang = 32767
    
    CALL my_subroutine
    
    CONTAINS
    
    SUBROUTINE my_subroutine
    
        INTEGER, DIMENSION(nang) :: fxhv
        print *, size(fxhv, kind = 1)
        print *, size(fxhv, kind = 2) 
        
    END SUBROUTINE my_subroutine

END PROGRAM arrays_04_size
    