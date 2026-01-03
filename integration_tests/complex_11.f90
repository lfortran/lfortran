SUBROUTINE CPDSA()
    COMPLEX :: C1
    C1 = DCMPLX(1.0D0, 6.0D0)
    print *, C1
    IF (REAL(C1) /= 1.0D0) ERROR STOP
    IF (AIMAG(C1) /= 6.0D0) ERROR STOP
ENDSUBROUTINE

program main
    call CPDSA()
end program
