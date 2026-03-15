program format_70
    use iso_fortran_env, only: dp => real64
    implicit none
    character(20) :: str
    character(67) :: str_long

    ! EN format: rounding causes mantissa overflow past [1, 1000)
    write(str, '(EN10.1)') 999.9999
    if (trim(adjustl(str)) /= "1.0E+03") error stop "EN10.1 999.9999 failed"

    write(str, '(EN10.1)') 99.96
    if (trim(adjustl(str)) /= "100.0E+00") error stop "EN10.1 99.96 failed"

    write(str, '(EN12.3)') 999.9999
    if (trim(adjustl(str)) /= "1.000E+03") error stop "EN12.3 999.9999 failed"

    write(str, '(EN12.3)') 0.0009999999
    if (trim(adjustl(str)) /= "1.000E-03") error stop "EN12.3 0.0009999999 failed"

    write(str, '(EN12.3)') 999999.6
    if (trim(adjustl(str)) /= "1.000E+06") error stop "EN12.3 999999.6 failed"

    ! Normal EN cases (no overflow)
    write(str, '(EN12.3)') 123.456
    if (trim(adjustl(str)) /= "123.456E+00") error stop "EN12.3 123.456 failed"

    write(str, '(EN12.3)') 0.00456
    if (trim(adjustl(str)) /= "4.560E-03") error stop "EN12.3 0.00456 failed"

    ! E format: high-precision rounding with > 15 significant digits
    write(str_long, '(E67.62)') 1.23456789101112e-62_dp
    if (str_long /= ".12345678910111198970867447229396098431025025677513665382815545E-61") &
        error stop "E67.62 rounding failed"
end program format_70
