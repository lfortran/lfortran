program format_112
    ! A negative zero keeps its minus sign in list-directed output and
    ! with every real edit descriptor (F, E, ES, EN, D, G).
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none
    character(len=60) :: s
    real :: x, p, nan
    double precision :: d
    complex :: c
    complex(kind=8) :: cd

    p = 0.0
    x = -p
    d = -0.0d0
    d = -abs(d)
    c = cmplx(x, x)
    cd = cmplx(d, d, kind=8)

    ! list-directed, real(4) and real(8)
    write(s, *) x
    if (adjustl(s) /= "-0.00000000") error stop "list-directed real(4)"
    write(s, *) d
    if (adjustl(s) /= "-0.0000000000000000") error stop "list-directed real(8)"

    ! list-directed, complex(4) and complex(8)
    write(s, *) c
    if (adjustl(s) /= "(-0.00000000,-0.00000000)") error stop "list-directed complex(4)"
    write(s, *) cd
    if (adjustl(s) /= "(-0.0000000000000000,-0.0000000000000000)") &
        error stop "list-directed complex(8)"
    write(s, *) cmplx(x, p)
    if (adjustl(s) /= "(-0.00000000,0.00000000)") error stop "list-directed mixed complex"

    ! edit descriptors, real(4)
    write(s, '(F8.3)') x
    if (s /= "  -0.000") error stop "F real(4)"
    write(s, '(F4.2)') x
    if (s /= "-.00") error stop "F narrow real(4)"
    write(s, '(E12.4)') x
    if (s /= " -0.0000E+00") error stop "E real(4)"
    write(s, '(ES12.4)') x
    if (s /= " -0.0000E+00") error stop "ES real(4)"
    write(s, '(EN12.4)') x
    if (s /= " -0.0000E+00") error stop "EN real(4)"
    write(s, '(G12.4)') x
    if (s /= "  -0.000    ") error stop "G real(4)"
    write(s, '(G0)') x
    if (s /= "-0.00000000") error stop "G0 real(4)"

    ! edit descriptors, real(8)
    write(s, '(F8.3)') d
    if (s /= "  -0.000") error stop "F real(8)"
    write(s, '(E12.4)') d
    if (s /= " -0.0000E+00") error stop "E real(8)"
    write(s, '(ES12.4)') d
    if (s /= " -0.0000E+00") error stop "ES real(8)"
    write(s, '(EN12.4)') d
    if (s /= " -0.0000E+00") error stop "EN real(8)"
    write(s, '(D12.4)') d
    if (s /= " -0.0000D+00") error stop "D real(8)"
    write(s, '(G12.4)') d
    if (s /= "  -0.000    ") error stop "G real(8)"

    ! edit descriptors, complex
    write(s, '(2F7.2)') c
    if (s /= "  -0.00  -0.00") error stop "F complex(4)"
    write(s, '(2ES11.3)') cd
    if (s /= " -0.000E+00 -0.000E+00") error stop "ES complex(8)"

    ! SP must not add a plus sign in front of the minus sign
    write(s, '(SP,F7.2,ES11.3,EN11.3)') x, x, x
    if (s /= "  -0.00 -0.000E+00 -0.000E+00") error stop "SP negative zero"
    write(s, '(SP,F7.2,ES11.3,EN11.3)') p, p, p
    if (s /= "  +0.00 +0.000E+00 +0.000E+00") error stop "SP positive zero"

    ! a literal negative zero
    write(s, *) -0.0
    if (adjustl(s) /= "-0.00000000") error stop "list-directed literal"

    ! a negative value that rounds to zero keeps its sign
    write(s, '(F6.2)') -0.001
    if (s /= " -0.00") error stop "F rounds to zero"
    write(s, '(F6.2)') -0.001d0
    if (s /= " -0.00") error stop "F rounds to zero real(8)"

    ! a positive zero has no sign
    write(s, *) p
    if (adjustl(s) /= "0.00000000") error stop "list-directed positive zero"
    write(s, '(F8.3,ES12.4,E12.4,EN12.4)') p, p, p, p
    if (s /= "   0.000  0.0000E+00  0.0000E+00  0.0000E+00") error stop "positive zero"

    ! a negative NaN is printed without a sign
    nan = ieee_value(nan, ieee_quiet_nan)
    nan = -nan
    write(s, *) nan
    if (adjustl(s) /= "NaN") error stop "list-directed NaN"
    write(s, '(F8.3,ES12.4)') nan, nan
    if (s /= "     NaN         NaN") error stop "formatted NaN"

    print *, x, d, c
end program
