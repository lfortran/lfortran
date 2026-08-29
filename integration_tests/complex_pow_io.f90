program complex_pow_io
    implicit none
    integer, parameter :: dp = kind(0.d0)
    complex(dp) :: c, z


    c = cmplx(-0.75_dp, 0.1_dp, dp)
    z = (0.0_dp, 0.0_dp)
    
    z = z**2 + c
    
    if (abs(z%re - (-0.75_dp)) > 1e-10 .or. abs(z%im - 0.1_dp) > 1e-10) then
        error stop 1
    end if


    write(*, "(a)", advance="no") "A"
    write(*, "(a)", advance="no") " "
    write(*, "(a)") "B"
end program complex_pow_io