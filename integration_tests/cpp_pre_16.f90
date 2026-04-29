program cpp_pre_16
    implicit none
    integer :: x

#if 1 /* taken */ || defined(__NOTDEFINED_MACRO__) /* not defined */
    x = 42
#else
#error This branch must not be selected
#endif

#if 0 /* not taken */
#error This #error must be ignored because branch is not taken
#warning This #warning must be ignored because branch is not taken
#endif

# /* a CPP null directive with a C-style comment */

    if (x /= 42) error stop
    print *, x
end program
