program real128_format_01
    ! E, ES, D and F editing of real(16) values at full precision
    implicit none
    real(16) :: x, z, third, big, small, neg, t
    character(60) :: s

    x = 1.5_16
    z = 0.0_16
    third = 1.0_16 / 3.0_16
    big = huge(x)
    small = tiny(x)
    neg = 0.0_16 - 123.456_16
    t = 9.9995_16

    write(s, '(E15.6)') third;          if (s /= '   0.333333E+00') error stop 1
    write(s, '(E15.6E3)') third;        if (s /= '  0.333333E+000') error stop 2
    write(s, '(ES15.6)') third;         if (s /= '   3.333333E-01') error stop 3
    write(s, '(ES15.6E4)') big;         if (s /= ' 1.189731E+4932') error stop 4
    write(s, '(ES15.6E4)') small;       if (s /= ' 3.362103E-4932') error stop 5
    write(s, '(E0.5)') third;           if (s /= '0.33333E+0000') error stop 6
    write(s, '(ES0.5)') neg;            if (s /= '-1.23456E+0002') error stop 7
    write(s, '(D15.6)') third;          if (s /= '   0.333333D+00') error stop 8
    write(s, '(F12.6)') third;          if (s /= '    0.333333') error stop 9
    write(s, '(F12.6)') neg;            if (s /= ' -123.456000') error stop 10
    write(s, '(F0.4)') third;           if (s /= '.3333') error stop 11
    write(s, '(F0.4)') x;               if (s /= '1.5000') error stop 12
    write(s, '(F8.3)') z;               if (s /= '   0.000') error stop 13
    write(s, '(E12.4)') z;              if (s /= '  0.0000E+00') error stop 14
    write(s, '(ES12.4)') z;             if (s /= '  0.0000E+00') error stop 15
    write(s, '(F10.2)') t;              if (s /= '     10.00') error stop 16
    write(s, '(F10.3)') t;              if (s /= '    10.000') error stop 17
    write(s, '(E10.3)') t;              if (s /= ' 0.100E+02') error stop 18
    write(s, '(ES10.3)') t;             if (s /= ' 1.000E+01') error stop 19
    write(s, '(F6.3)') neg;             if (s /= '******') error stop 20
    write(s, '(E9.3)') 0.0_16 - third;  if (s /= '-.333E+00') error stop 21
    write(s, '(E8.3)') third;           if (s /= '.333E+00') error stop 22
    write(s, '(E7.3)') third;           if (s /= '*******') error stop 23
    write(s, '(SP,ES12.3)') third;      if (s /= '  +3.333E-01') error stop 24
    write(s, '(F40.34)') third
    if (s /= '    0.3333333333333333333333333333333333') error stop 25
    write(s, '(ES40.33)') third
    if (s /= ' 3.333333333333333333333333333333333E-01') error stop 26
    write(s, '(F25.2)') x * 1.0e20_16;  if (s /= ' 150000000000000000000.00') error stop 27
    write(s, '(2PE14.5)') third;        if (s /= '   33.3333E-02') error stop 28
    write(s, '(-1PE14.5)') third;       if (s /= '   0.03333E+01') error stop 29
    write(s, '(1PE14.5)') third;        if (s /= '   3.33333E-01') error stop 30
    write(s, '(RD,F10.4)') third;       if (s /= '    0.3333') error stop 31
    write(s, '(RU,F10.4)') third;       if (s /= '    0.3334') error stop 32
    write(s, '(RZ,ES10.2)') 0.0_16 - third; if (s /= ' -3.33E-01') error stop 33
    write(s, '(E12.4)') 1.0e-100_16;    if (s /= '  0.1000E-99') error stop 34
    write(s, '(F5.1)') 0.0_16 - 0.5_16; if (s /= ' -0.5') error stop 35
    write(s, '(ES15.6E4)') 0.0_16 - big; if (s /= '-1.189731E+4932') error stop 36
    write(s, '(E12.3E4,ES11.2E4)') epsilon(x), epsilon(x)
    if (s /= ' 0.193E-0033 1.93E-0034') error stop 37
    print *, "ok"
end program
