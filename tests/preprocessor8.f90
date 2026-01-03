program preprocessor8
! pre-defined macros
implicit none

print *, __LFORTRAN__
#ifdef __VERSION__
print *, "__VERSION__ present"
#endif
#ifdef __LFORTRAN_MAJOR__
print *, "__LFORTRAN_MAJOR__ present"
#endif
#ifdef __LFORTRAN_MINOR__
print *, "__LFORTRAN_MINOR__ present"
#endif
#ifdef __LFORTRAN_PATCHLEVEL__
print *, "__LFORTRAN_PATCHLEVEL__ present"
#endif
print *, __FILE__
print *, __LINE__
print *, __LINE__

end program
