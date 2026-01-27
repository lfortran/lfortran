program test_stringify
    implicit none

#ifndef X                /* ABC-comment */ 
#ifdef __GFORTRAN__      /* ABC-comment */
#define X 4
#else                    /* XYZ-comment */
#define X 5
#endif
#endif

    print *, X
end program test_stringify
