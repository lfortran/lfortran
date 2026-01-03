program cpp_pre_03
#ifdef __ELF__
    print *,"ELF"
#endif
#ifdef __x86_64__
    print *,"x86_64"
#endif
#ifdef __i386__
    print *,"i386"
#endif
#ifdef __SIZEOF_POINTER__
    print *,"SIZEOF_POINTER", __SIZEOF_POINTER__
#endif
#ifdef __SIZEOF_SIZE_T__
    print *,"SIZEOF_SIZE_T",__SIZEOF_SIZE_T__
#endif
print *, "Ok"
end program
