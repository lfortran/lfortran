program cpp_pre_03
#ifdef _WIN32
    print *,"Windows"
#endif
#ifdef __linux__
    print *,"Linux"
#endif
#ifdef __APPLE__
    print *,"OSX"
#endif
#ifdef __aarch64__
    print *,"Apple ARM"
#endif
#ifdef __x86_64__
    print *,"Apple AMD"
#endif
#ifdef __FreeBSD__
    print *,"FreeBSD"
#endif
#ifdef __OpenBSD__
    print *,"OpenBSD"
#endif
print *, "Ok"
end program
