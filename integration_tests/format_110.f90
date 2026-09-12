program format_110
    character(14), save :: f1 = '(SS,ESxx.xxE4)'
    character(13) :: result
    character(13), dimension(5) :: expected
    real, allocatable :: x(:)
    integer :: i, dmx

    x = [1234.5678, 8765.4321, 0.0, 30.10e35, -123.45]
    dmx = 5

    expected = [ &
        ' 1.2346E+0003', &
        ' 8.7654E+0003', &
        ' 0.0000E+0000', &
        ' 3.0100E+0036', &
        '-1.2345E+0002'  &
    ]

    write(f1(7:11), '(SS,I2,".",I2.2)') dmx + 8, dmx - 1

    do i = 1, size(x)
        write(result, f1) x(i)

        if (result /= expected(i)) then
            error stop
        end if
    end do

end program format_110