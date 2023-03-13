PROGRAM main
    INTEGER :: x
    x = 42
    CALL try_to_change(x)
END PROGRAM

SUBROUTINE try_to_change(y)
    INTEGER, INTENT(IN) :: y
    y = 99  
END SUBROUTINE
