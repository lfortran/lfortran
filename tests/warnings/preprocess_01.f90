program preprocess_01
    implicit none
#line 10

    integer :: x
#ifdef pqr
x = (2+3)*5
#elif mno == 18
x = 18
#else
    print *, "Else condition"
    x = 30
#endif
print *, x
x = 10000
#define abc(m, n) m + n

#abcd

end program
