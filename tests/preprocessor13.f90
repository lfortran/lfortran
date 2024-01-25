#if defined(__GFORTRAN__)
print *, "GFortran"
#elif defined(__LFORTRAN__)
print *, "LFortran"
#else
print *, "Other"
#endif
end
