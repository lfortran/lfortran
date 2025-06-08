program preprocessor21

#if (!defined(VAR1) | !defined(VAR2))
    print *, "Atleast one of VAR1 and VAR2 isn't defined"
#else
    print *, 'Both VAR1 and VAR2 are defined.'
#endif

#if (!defined(VAR3) & !defined(VAR4))
    print *, "Neither of VAR3 or VAR4 is defined"
#else
    print *, "At least one of VAR3 or VAR4 is defined."
#endif

#if (!defined(VAR5) ^ !defined(VAR6))
   print *, "Exactly one of VAR5 or VAR6 is not defined."
#else
   print *, "Both VAR5 and VAR6 are defined or neither is defined."
#endif

end program preprocessor21
