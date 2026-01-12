program cpp_ifdef_comment
    implicit none
#ifdef __GFORTRAN__      /* ABC-comment */
#define X 4
#else                      /* XYZ-comment */
#define X 5
#endif
    print *, X
end program cpp_ifdef_comment