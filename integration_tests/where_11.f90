MODULE where_11_mod
    IMPLICIT NONE
    INTEGER :: a(3)
    integer :: b(3) 
    
    CONTAINS
    
    SUBROUTINE test_01
        where(abs(a) == 1) b = 555
        print *, b
        if (any(b /= [555, 555 , 0])) error stop
        b = 0
    END SUBROUTINE test_01
    
    SUBROUTINE test_02
        where(abs(a)*1 == 1) b = 556
        print *, b
        if (any(b /= [556, 556 , 0])) error stop
        b = 0
    END SUBROUTINE test_02
    
    SUBROUTINE test_03
        where(abs(a)*1 + 1 == 2) b = 557
        print *, b
        if (any(b /= [557, 557 , 0])) error stop
        b = -1
        
    END SUBROUTINE test_03
    
    SUBROUTINE test_04
        where(min(a, b) == -1 ) b = 558
        print *, b
        if (any(b /= [558, 558 , 558])) error stop
        b = 0
    END SUBROUTINE test_04
END MODULE where_11_mod
    
PROGRAM where_11
    USE where_11_mod
    implicit none
    a = [1,-1,2]
    b = 0
    CALL test_01
    CALL test_02
    CALL test_03
    CALL test_04

END PROGRAM where_11
