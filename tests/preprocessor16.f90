program test_cpp_hex
#if 0xFF == 255
print *, "yes hex and int equal"
#else
print *, "no hex and int equal"
#endif

#if 0xa == 0b1010
print *, "yes hex and bin equal"
#else
print *, "no hex and bin not equal"
#endif


#if 0B1010 == 10
print *, "yes bin and int equal"
#else
print *, "no bin and int not equal"
#endif

end program
