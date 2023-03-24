SUBROUTINE CPDSA()
    DOUBLE PRECISION PD
    COMPLEX C
    PD = 1.0D0
    C = DCMPLX(PD, 0.0D0)
    PRINT *, C
    if (REAL(C) /= 1.0D0) ERROR STOP
    if (AIMAG(C) /= 0.0D0) ERROR STOP
END SUBROUTINE

program main
    call CPDSA()
end program
