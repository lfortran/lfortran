program cpp_flag_01
#if defined(CPP_FLAG_01)
    print *, "ok"
#else
    error stop 1
#endif
end program cpp_flag_01
