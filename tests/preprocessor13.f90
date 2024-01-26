#if defined(__GFORTRAN__)
print *, "1 GFortran"
#elif defined(__LFORTRAN__)
print *, "1 LFortran"
#else
print *, "1 Other"
#endif

#if defined(__GFORTRAN__)
print *, "2 GFortran"
#elif defined(__LFORTRAN__)
print *, "2 LFortran 1"
#elif defined(__LFORTRAN__)
print *, "2 LFortran 2"
#else
print *, "2 Other"
#endif

#if defined(__GFORTRAN__)
print *, "3 GFortran 1"
#elif defined(__GFORTRAN__)
print *, "3 GFortran 2"
#elif defined(__LFORTRAN__)
print *, "3 LFortran 1"
#elif defined(__LFORTRAN__)
print *, "3 LFortran 2"
#else
print *, "3 Other"
#endif

#if defined(__GFORTRAN__)
print *, "4 GFortran 1"
#elif defined(__GFORTRAN__)
print *, "4 GFortran 2"

#  if defined(__GFORTRAN__)
    print *, "41 GFortran 1"
#  elif defined(__GFORTRAN__)
    print *, "41 GFortran 2"
#  elif defined(__LFORTRAN__)
    print *, "41 LFortran 1"
#  elif defined(__LFORTRAN__)
    print *, "41 LFortran 2"
#  else
    print *, "41 Other"
#  endif

#elif defined(__LFORTRAN__)
print *, "4 LFortran 1"

#  if defined(__GFORTRAN__)
    print *, "42 GFortran 1"
#  elif defined(__GFORTRAN__)
    print *, "42 GFortran 2"
#  elif defined(__LFORTRAN__)
    print *, "42 LFortran 1"
#  elif defined(__LFORTRAN__)
    print *, "42 LFortran 2"
#  else
    print *, "42 Other"
#  endif

#elif defined(__LFORTRAN__)
print *, "4 LFortran 2"

#  if defined(__GFORTRAN__)
    print *, "43 GFortran 1"
#  elif defined(__GFORTRAN__)
    print *, "43 GFortran 2"
#  elif defined(__LFORTRAN__)
    print *, "43 LFortran 1"
#  elif defined(__LFORTRAN__)
    print *, "43 LFortran 2"
#  else
    print *, "43 Other"
#  endif

#else
print *, "4 Other"

#  if defined(__GFORTRAN__)
    print *, "44 GFortran 1"
#  elif defined(__GFORTRAN__)
    print *, "44 GFortran 2"
#  elif defined(__LFORTRAN__)
    print *, "44 LFortran 1"
#  elif defined(__LFORTRAN__)
    print *, "44 LFortran 2"
#  else
    print *, "44 Other"
#  endif

#endif

end
