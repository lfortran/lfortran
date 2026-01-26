program format_50
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: tiny_val, huge_val, test_val
    character(20) :: str

    tiny_val = tiny(1.0_dp)
    huge_val = huge(1.0_dp)
    test_val = 1.23456789e10_dp

    ! Test ES format with E3
    print "(A,ES11.3E3)", 'tiny =', tiny_val
    print "(A,ES11.3E3)", 'huge =', huge_val
    print "(A,ES11.3E3)", 'test =', test_val

    ! Test EN format with E3
    print "(A,EN11.3E3)", 'test =', test_val

    ! Test E format with E3
    print "(A,E11.3E3)", 'test =', test_val

    ! Test ES format with E4
    print "(A,ES12.3E4)", 'tiny =', tiny_val
    print "(A,ES12.3E4)", 'test =', test_val

    ! Verify with write statements - check that 'E' is present
    write(str, "(ES11.3E3)") tiny_val
    if (index(trim(str), 'E') == 0) error stop "ES11.3E3 tiny missing E"
    if (trim(adjustl(str)) /= "2.225E-308") error stop "ES11.3E3 tiny failed"

    write(str, "(ES11.3E3)") huge_val
    if (index(trim(str), 'E') == 0) error stop "ES11.3E3 huge missing E"
    if (trim(adjustl(str)) /= "1.798E+308") error stop "ES11.3E3 huge failed"

    write(str, "(ES11.3E3)") test_val
    if (index(trim(str), 'E') == 0) error stop "ES11.3E3 test missing E"
    if (trim(adjustl(str)) /= "1.235E+010") error stop "ES11.3E3 test failed"

    write(str, "(EN11.3E3)") test_val
    if (index(trim(str), 'E') == 0) error stop "EN11.3E3 test missing E"
    if (trim(adjustl(str)) /= "12.346E+009") error stop "EN11.3E3 test failed"

    write(str, "(E11.3E3)") test_val
    if (index(trim(str), 'E') == 0) error stop "E11.3E3 test missing E"
    if (trim(adjustl(str)) /= "0.123E+011") error stop "E11.3E3 test failed"

    write(str, "(ES12.3E4)") tiny_val
    if (index(trim(str), 'E') == 0) error stop "ES12.3E4 tiny missing E"
    if (trim(adjustl(str)) /= "2.225E-0308") error stop "ES12.3E4 tiny failed"

    write(str, "(ES12.3E4)") test_val
    if (index(trim(str), 'E') == 0) error stop "ES12.3E4 test missing E"
    if (trim(adjustl(str)) /= "1.235E+0010") error stop "ES12.3E4 test failed"

    print *, "PASSED"
end program format_50
