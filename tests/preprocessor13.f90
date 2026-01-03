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


#define X 1
#define Y 1

#if X == 1
print *, "X is 1"
#   if Y == 1
    print *, "1 Y is 1"
#   elif Y == 2
    print *, "1 Y is 2"
#   elif Y == 3
    print *, "1 Y is 3"
#   elif Y == 4
    print *, "1 Y is 4"
#   else
    print *, "1 Y is not 1-4"
#   endif
#elif X == 2
print *, "X is 2"
#   if Y == 1
    print *, "2 Y is 1"
#   elif Y == 2
    print *, "2 Y is 2"
#   elif Y == 3
    print *, "2 Y is 3"
#   elif Y == 4
    print *, "2 Y is 4"
#   else
    print *, "2 Y is not 1-4"
#   endif
#elif X == 3
print *, "X is 3"
#   if Y == 1
    print *, "3 Y is 1"
#   elif Y == 2
    print *, "3 Y is 2"
#   elif Y == 3
    print *, "3 Y is 3"
#   elif Y == 4
    print *, "3 Y is 4"
#   else
    print *, "3 Y is not 1-4"
#   endif
#elif X == 4
print *, "X is 4"
#   if Y == 1
    print *, "4 Y is 1"
#   elif Y == 2
    print *, "4 Y is 2"
#   elif Y == 3
    print *, "4 Y is 3"
#   elif Y == 4
    print *, "4 Y is 4"
#   else
    print *, "4 Y is not 1-4"
#   endif
#else
print *, "X is not 1-4"
#   if Y == 1
    print *, "5 Y is 1"
#   elif Y == 2
    print *, "5 Y is 2"
#   elif Y == 3
    print *, "5 Y is 3"
#   elif Y == 4
    print *, "5 Y is 4"
#   else
    print *, "5 Y is not 1-4"
#   endif
#endif

#define X 2
#define Y 3

#if X == 1
print *, "X is 1"
#   if Y == 1
    print *, "1 Y is 1"
#   elif Y == 2
    print *, "1 Y is 2"
#   elif Y == 3
    print *, "1 Y is 3"
#   elif Y == 4
    print *, "1 Y is 4"
#   else
    print *, "1 Y is not 1-4"
#   endif
#elif X == 2
print *, "X is 2"
#   if Y == 1
    print *, "2 Y is 1"
#   elif Y == 2
    print *, "2 Y is 2"
#   elif Y == 3
    print *, "2 Y is 3"
#   elif Y == 4
    print *, "2 Y is 4"
#   else
    print *, "2 Y is not 1-4"
#   endif
#elif X == 3
print *, "X is 3"
#   if Y == 1
    print *, "3 Y is 1"
#   elif Y == 2
    print *, "3 Y is 2"
#   elif Y == 3
    print *, "3 Y is 3"
#   elif Y == 4
    print *, "3 Y is 4"
#   else
    print *, "3 Y is not 1-4"
#   endif
#elif X == 4
print *, "X is 4"
#   if Y == 1
    print *, "4 Y is 1"
#   elif Y == 2
    print *, "4 Y is 2"
#   elif Y == 3
    print *, "4 Y is 3"
#   elif Y == 4
    print *, "4 Y is 4"
#   else
    print *, "4 Y is not 1-4"
#   endif
#else
print *, "X is not 1-4"
#   if Y == 1
    print *, "5 Y is 1"
#   elif Y == 2
    print *, "5 Y is 2"
#   elif Y == 3
    print *, "5 Y is 3"
#   elif Y == 4
    print *, "5 Y is 4"
#   else
    print *, "5 Y is not 1-4"
#   endif
#endif


#define X 4
#define Y 4

#if X == 1
print *, "X is 1"
#   if Y == 1
    print *, "1 Y is 1"
#   elif Y == 2
    print *, "1 Y is 2"
#   elif Y == 3
    print *, "1 Y is 3"
#   elif Y == 4
    print *, "1 Y is 4"
#   else
    print *, "1 Y is not 1-4"
#   endif
#elif X == 2
print *, "X is 2"
#   if Y == 1
    print *, "2 Y is 1"
#   elif Y == 2
    print *, "2 Y is 2"
#   elif Y == 3
    print *, "2 Y is 3"
#   elif Y == 4
    print *, "2 Y is 4"
#   else
    print *, "2 Y is not 1-4"
#   endif
#elif X == 3
print *, "X is 3"
#   if Y == 1
    print *, "3 Y is 1"
#   elif Y == 2
    print *, "3 Y is 2"
#   elif Y == 3
    print *, "3 Y is 3"
#   elif Y == 4
    print *, "3 Y is 4"
#   else
    print *, "3 Y is not 1-4"
#   endif
#elif X == 4
print *, "X is 4"
#   if Y == 1
    print *, "4 Y is 1"
#   elif Y == 2
    print *, "4 Y is 2"
#   elif Y == 3
    print *, "4 Y is 3"
#   elif Y == 4
    print *, "4 Y is 4"
#   else
    print *, "4 Y is not 1-4"
#   endif
#else
print *, "X is not 1-4"
#   if Y == 1
    print *, "5 Y is 1"
#   elif Y == 2
    print *, "5 Y is 2"
#   elif Y == 3
    print *, "5 Y is 3"
#   elif Y == 4
    print *, "5 Y is 4"
#   else
    print *, "5 Y is not 1-4"
#   endif
#endif

#define X 5
#define Y 5

#if X == 1
print *, "X is 1"
#   if Y == 1
    print *, "1 Y is 1"
#   elif Y == 2
    print *, "1 Y is 2"
#   elif Y == 3
    print *, "1 Y is 3"
#   elif Y == 4
    print *, "1 Y is 4"
#   else
    print *, "1 Y is not 1-4"
#   endif
#elif X == 2
print *, "X is 2"
#   if Y == 1
    print *, "2 Y is 1"
#   elif Y == 2
    print *, "2 Y is 2"
#   elif Y == 3
    print *, "2 Y is 3"
#   elif Y == 4
    print *, "2 Y is 4"
#   else
    print *, "2 Y is not 1-4"
#   endif
#elif X == 3
print *, "X is 3"
#   if Y == 1
    print *, "3 Y is 1"
#   elif Y == 2
    print *, "3 Y is 2"
#   elif Y == 3
    print *, "3 Y is 3"
#   elif Y == 4
    print *, "3 Y is 4"
#   else
    print *, "3 Y is not 1-4"
#   endif
#elif X == 4
print *, "X is 4"
#   if Y == 1
    print *, "4 Y is 1"
#   elif Y == 2
    print *, "4 Y is 2"
#   elif Y == 3
    print *, "4 Y is 3"
#   elif Y == 4
    print *, "4 Y is 4"
#   else
    print *, "4 Y is not 1-4"
#   endif
#else
print *, "X is not 1-4"
#   if Y == 1
    print *, "5 Y is 1"
#   elif Y == 2
    print *, "5 Y is 2"
#   elif Y == 3
    print *, "5 Y is 3"
#   elif Y == 4
    print *, "5 Y is 4"
#   else
    print *, "5 Y is not 1-4"
#   endif
#endif

end
