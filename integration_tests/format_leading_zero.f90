program format_leading_zero
    implicit none
    character(80) :: s, default_s, other
    character(6) :: records(3)
    character(20) :: input
    real :: x, y, z
    integer :: ios
    logical :: flag

    ! F2023 13.8.5. GFortran 16 does not yet support these descriptors.
    write(s, '(LZP,F0.3,LZS,F0.3)') .5, .5
    call check(s, '0.500.500')
    write(default_s, '(F0.3)') .5
    write(s, '(LZS,F0.3,LZP,F0.3,LZ,F0.3)') .5, .5, .5
    call check(s, '.5000.500' // trim(default_s))
    write(s, '(F0.3)') .5
    call check(s, trim(default_s))
    write(s, '(lzp,2(f0.2,1x),lzs,f0.2)') .5, -.5, .5
    call check(s, '0.50 -0.50 .50')

    write(s, '(LZP,E12.3,LZS,E12.3)') .5, .5
    call check(s, '   0.500E+00    .500E+00')
    write(s, '(LZP,D12.3,LZS,D12.3)') .5, .5
    call check(s, '   0.500D+00    .500D+00')
    write(s, '(LZP,G12.3,LZS,G12.3)') .5, .5
    call check(s, '   0.500        .500    ')
    write(s, '(LZP,G12.3,LZS,G12.3)') .0625, .0625
    call check(s, '   0.625E-01    .625E-01')
    write(default_s, '(G26.15)') 0.01234567890123456_8
    call check(default_s, '     0.123456789012346E-01')
    write(s, '(LZP,G26.15,LZS,G26.15)') 0.01234567890123456_8, 0.01234567890123456_8
    call check(s, '     0.123456789012346E-01      .123456789012346E-01')
    write(s, '(LZS,LZ,G26.15)') 0.01234567890123456_8
    call check(s, trim(default_s))
    write(default_s, '(G26.16)') 0.12345678901234564_8
    call check(default_s, '    0.1234567890123456')
    write(s, '(LZP,G26.16,LZS,G26.16)') 0.12345678901234564_8, 0.12345678901234564_8
    call check(s, '    0.1234567890123456         .1234567890123456')
    write(s, '(LZS,LZ,G26.16)') 0.12345678901234564_8
    call check(s, trim(default_s))
    write(s, '(LZP,RU,G26.16,LZS,RD,G26.16)') 0.1_8, -0.1_8
    call check(s, '    0.1000000000000001        -.1000000000000001')
    write(s, '(LZP,RC,G12.2,LZS,G12.2)') 0.125_8, -0.125_8
    call check(s, '    0.13        -.13')
    write(s, '(LZP,RC,G26.4,LZS,G26.4)') 0.015625_8, -0.015625_8
    call check(s, '                0.1563E-01                -.1563E-01')
    write(s, '(LZP,RU,RC,G12.2,LZS,RN,G12.2)') 0.125_8, -0.125_8
    call check(s, '    0.13        -.12')
    write(s, '(LZP,RC,RN,G26.4,LZS,G26.4)') 0.015625_8, -0.015625_8
    call check(s, '                0.1562E-01                -.1562E-01')
    write(default_s, '(LZP,G12.2,LZS,G26.4)') 0.125_8, -0.015625_8
    write(s, '(LZP,RC,RP,G12.2,LZS,G26.4)') 0.125_8, -0.015625_8
    call check(s, trim(default_s))
    write(s, '(LZP,DC,G0.17,LZS,G0.17)') 0.12345678901234564_8, -0.12345678901234564_8
    call check(s, '0,12345678901234564-,12345678901234564')
    write(s, '(LZP,G21.16,LZS,G21.16)') 0.12345678901234564_8, 0.12345678901234564_8
    call check(s, '*********************.1234567890123456    ')
    write(s, '(LZP,SP,3P,G10.3E2,LZS,RD,G10.3E2)') .9999_8, -.9999_8
    call check(s, ' +1.00     -1.00    ')
    write(s, '(LZP,RU,G12.2,LZS,RD,G12.2)') .0999, .0999
    call check(s, '    0.10         .99E-01')
    write(s, '(LZP,SP,G0,LZS,G0)') .5, .5
    call check(s, '+0.500000000+.500000000')
    write(s, '(LZP,E0.3,LZS,E0.3)') .5, .5
    call check(s, '0.500E+0.500E+0')
    write(s, '(LZP,1P,E12.3,LZS,E12.3)') .5, .5
    call check(s, '   5.000E-01   5.000E-01')

    write(s, '(LZP,F4.3,LZS,F4.3,LZP,E8.3,LZS,E8.3)') .5, .5, .5, .5
    call check(s, '****.500********.500E+00')
    write(s, '(LZP,G8.3,LZS,G8.3,LZP,G8.3,LZS,G8.3)') .5, .5, .0625, .0625
    call check(s, '********.500    ********.625E-01')
    write(s, '(LZP,SP,F6.2,LZS,F6.2,SS,F6.2)') .5, .5, -.5
    call check(s, ' +0.50  +.50  -.50')
    write(s, '(LZS,F0.0,LZP,F0.0,LZS,F0.2)') 0., 0., .999
    call check(s, '0.0.1.00')
    write(s, '(LZP,F0.2,LZS,F0.2)') -0.0, 0.0
    call check(s, '-0.00.00')
    write(s, '(LZP,RU,F0.2,LZS,RD,F0.2,DC,LZP,F0.2)') .0625, .0625, .5
    call check(s, '0.07.060,50')

    write(s, '(EN12.3,ES12.3)') .5, .5
    write(other, '(LZS,EN12.3,LZP,ES12.3)') .5, .5
    call check(other, trim(s))
    write(s, '(LZP,L1,Z2,I2,LZS,F0.1)') .true., 15, 3, .5
    call check(s, 'T F 3.5')
    input = '0.5 0.5 0.5 T'
    read(input, '(LZP,F4.1,LZS,F4.1,LZ,F4.1,L1)') x, y, z, flag
    if (x /= .5 .or. y /= .5 .or. z /= .5 .or. .not. flag) error stop
    input = ' 0.5 0.5 0.5T'
    read(input, '(L Z P,F4.1,l z s,F4.1,L Z,F4.1,L1)', iostat=ios) x, y, z, flag
    if (ios /= 0 .or. x /= .5 .or. y /= .5 .or. z /= .5 .or. .not. flag) error stop
    write(records, '(LZS,(F6.2))') .5, .5, .5
    call check(records(1), '   .50')
    call check(records(2), '   .50')
    call check(records(3), '   .50')
    ! LZ controls also execute without an effective data item.
    write(s, '(LZP,LZS,LZ)')
    call check(s, '')
contains
    subroutine check(actual, expected)
        character(*), intent(in) :: actual, expected
        if (actual /= expected) then
            print *, 'actual:   [', actual, ']'
            print *, 'expected: [', expected, ']'
            error stop
        end if
    end subroutine
end program
