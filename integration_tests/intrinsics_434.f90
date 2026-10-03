PROGRAM intrinsics_434
   INTEGER, PARAMETER :: ki = SELECTED_INT_KIND(r=9)
   WRITE(*,*) 'ki =', ki
   IF ( ki /= 4) ERROR STOP
END PROGRAM intrinsics_434