program test_complex_pow_precision
    implicit none
    integer, parameter :: dp = kind(0.d0)
    complex(dp) :: z, expected
    real(dp) :: error_re, error_im

    z = cmplx(-1.5_dp, 0.0_dp, dp)
    expected = cmplx(2.25_dp, 0.0_dp, dp)
    
    z = z**2
    
    error_re = abs(z%re - expected%re)
    error_im = abs(z%im - expected%im)
    
    if (error_re > 1.0e-15_dp .or. error_im > 0.0_dp) then
        print *, "Test Failed!"
        print *, "Expected: ", expected
        print *, "Got:      ", z
        print *, "Error Re: ", error_re
        print *, "Error Im: ", error_im
        error stop 1
    end if
    
    print *, "Test Passed!"
end program test_complex_pow_precision