SUBROUTINE sub(f)
    IMPLICIT DOUBLE PRECISION (A-H)
    EXTERNAL f
    H=HINIT853(f)
    RETURN
END
   
FUNCTION HINIT853(f)
    IMPLICIT DOUBLE PRECISION (A-H)
    RETURN
END 

program main 
    EXTERNAL f
    call sub(f)
end program