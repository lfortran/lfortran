program cpp_pre_17
    ! Regression: source files ending in `#endif` without a trailing
    ! newline must still preprocess successfully. Modelled after
    ! `json-fortran/test/jf_test_47.F90`.
    implicit none
    integer :: x
#ifndef NDEBUG
    x = 42
#else
    x = 0
#endif
    if (x /= 42) error stop
    print *, x
end program
#ifndef INTEGRATED_TESTS
!*****************************************************************************************
#endif