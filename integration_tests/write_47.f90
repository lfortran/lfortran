program write_47
    ! write to an internal file (character variable) and to units
    implicit none
    real :: x
    real(8) :: y
    integer :: i, ios
    integer(8) :: j
    integer(1) :: k
    logical :: l
    logical(1) :: l1
    complex :: z
    complex(8) :: zz
    character(20) :: s

    x = -1
    y = 2.5d0
    i = 42
    j = -7
    k = 5
    l = .true.
    l1 = .false.
    z = (1.5, -2.0)
    zz = (0.5d0, 0.25d0)

    ! List-directed: LFortran writes no leading blank by default and a single
    ! leading blank with --std=f23, GFortran writes a leading blank.
    write(s, *) x
    if (s /= "-1.00000000         " .and. s /= " -1.00000000        " &
        .and. s /= "  -1.00000000       ") error stop 1
    write(s, *) i
    if (s /= "42                  " .and. s /= " 42                 " &
        .and. s /= "          42        ") error stop 2
    write(s, *) l
    if (s /= "T                   " .and. s /= " T                  ") error stop 3

    write(s, '(f8.3)') x
    if (s /= "  -1.000            ") error stop 4
    write(s, '(f6.2,1x,i0)') y, i
    if (s /= "  2.50 42           ") error stop 5
    write(s, '(i5,i3)') j, k
    if (s /= "   -7  5            ") error stop 6
    write(s, '(l2,l3)') l, l1
    if (s /= " T  F               ") error stop 7
    write(s, '(2f5.1)') z
    if (s /= "  1.5 -2.0          ") error stop 8
    write(s, '(2f6.2)') zz
    if (s /= "  0.50  0.25        ") error stop 9
    write(s, '(a,"-",a)') "xy", "abc"
    if (s /= "xy-abc              ") error stop 10
    write(s, '(es12.4)') y
    if (s /= "  2.5000E+00        ") error stop 11

    ios = -1
    write(s, '(i0)', iostat=ios) i + 1
    if (ios /= 0) error stop 12
    if (s /= "43                  ") error stop 13

    write(*, *) x, i, "hi", l
    write(6, '(f8.3,a)') x, "|"
    write(*, '(a)') s
    ios = -1
    write(6, '(i0,1x,l1)', iostat=ios) i, l
    if (ios /= 0) error stop 14
end program
