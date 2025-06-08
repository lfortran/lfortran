! This test checks for handling different arrays sizes kind correctly with no runtime errors.
PROGRAM arrays_04_size

    IMPLICIT NONE
    
    INTEGER(1) :: nang1 = 127
    INTEGER(2) :: nang2 = 32767
    INTEGER(4) :: nang4 = 2147483647
    INTEGER(8) :: nang8 = 9223372036854775807_8
    
    CALL my_subroutine
    
    CONTAINS
    
    SUBROUTINE my_subroutine
    
        INTEGER, DIMENSION(nang1) :: fxhv1
        INTEGER, DIMENSION(nang2) :: fxhv2
        INTEGER, DIMENSION(nang4) :: fxhv4
        INTEGER, DIMENSION(nang8) :: fxhv8
        print *, size(fxhv1, kind = 1)
        print *, size(fxhv1, kind = 2)
        print *, size(fxhv1, kind = 4)
        print *, size(fxhv1, kind = 8)

        print *, size(fxhv2, kind = 1)
        print *, size(fxhv2, kind = 2)
        print *, size(fxhv2, kind = 4)
        print *, size(fxhv2, kind = 8)

        print *, size(fxhv4, kind = 1)
        print *, size(fxhv4, kind = 2)
        print *, size(fxhv4, kind = 4)
        print *, size(fxhv4, kind = 8)

        print *, size(fxhv8, kind = 1)
        print *, size(fxhv8, kind = 2)
        print *, size(fxhv8, kind = 4)
        print *, size(fxhv8, kind = 8)
        
    END SUBROUTINE my_subroutine

END PROGRAM arrays_04_size
    