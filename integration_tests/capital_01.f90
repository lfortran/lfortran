PROGRAM CAPITAL_01
    INTEGER A,B,C
    A=1
    B=2
    C=A+B
    PRINT *, C
    IF ( A /= 1 ) ERROR STOP
    IF ( B /= 2 ) ERROR STOP
    IF ( C /= 3 ) ERROR STOP
END
