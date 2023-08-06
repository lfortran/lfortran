! File test.f90
program test
#ifdef SOMETHING
#endif // This should work

# define square(a) /* This should be ignored as well */ a * a
#define abc "ABC"
    integer :: a = 10 /*
    hi*/

    /*
        This is a multi-line comment
        line 2*** /**
        line 3
    */
    print *, /*hi, "abc\n"*/ square(a), "hi /* abc*/  //abc"
    print *, "asdjfal" // "djafl"
    print *, abc
end program test
