program formatted_read_03
    implicit none

    complex :: c1, c2, c3
    character(30) :: cdata

    cdata = '1.05.522.066.633.123455.0789'
    read (cdata, 100) c1, c2, c3
100 format (2(F3.1), 2(F4.1), 2(F7.4))

    if (abs(real(c1) - 1.0) > 1e-5 .or. abs(aimag(c1) - 5.5) > 1e-5) then
        error stop 'c1 mismatch'
    end if
    if (abs(real(c2) - 22.0) > 1e-5 .or. abs(aimag(c2) - 66.6) > 1e-5) then
        error stop 'c2 mismatch'
    end if
    if (abs(real(c3) - 33.1234) > 1e-5 .or. abs(aimag(c3) - 55.0789) > 1e-5) then
        error stop 'c3 mismatch'
    end if
end program formatted_read_03