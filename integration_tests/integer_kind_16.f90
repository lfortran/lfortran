program integer_kind_16
    use integer_kind_16_mod
    implicit none
    integer, parameter :: k19 = selected_int_kind(19)
    integer, parameter :: k38 = selected_int_kind(38)
    integer, parameter :: k39 = selected_int_kind(39)
    integer(16), parameter :: p_huge = 170141183460469231731687303715884105727_16
    integer(16), parameter :: p70 = 2_16 ** 70
    integer(16), parameter :: neg_arr(2) = -mod_arr
    integer(16) :: x, y, h, built, wide
    integer(k19) :: via_kind
    integer(8) :: i8
    integer(4) :: i4, s70, s68, s4
    real(8) :: r8
    integer :: r, i
    character(len=80) :: s
    interface
        integer(16) function add16(a, b) bind(c)
            integer(16), value :: a, b
        end function add16
    end interface

    if (k19 /= 16) error stop
    if (k38 /= 16) error stop
    if (k39 /= -1) error stop
    r = 18
    if (selected_int_kind(r) /= 8) error stop
    r = 19
    if (selected_int_kind(r) /= 16) error stop
    r = 38
    if (selected_int_kind(r) /= 16) error stop
    r = 39
    if (selected_int_kind(r) /= -1) error stop

    if (kind(x) /= 16) error stop
    if (digits(x) /= 127) error stop
    if (range(x) /= 38) error stop
    if (bit_size(x) /= 128) error stop

    x = 10
    y = 3
    if (x + y /= 13) error stop
    if (x - y /= 7) error stop
    if (x * y /= 30) error stop
    if (x / y /= 3) error stop
    if (mod(x, y) /= 1) error stop
    if (.not. (x > y)) error stop
    if (x == y) error stop
    if (.not. (-2_16 < -1_16)) error stop
    if (abs(-5_16) /= 5) error stop
    if (2_16 ** 10 /= 1024) error stop

    i8 = -99
    x = i8
    if (x /= -99_16) error stop
    i4 = x
    if (i4 /= -99) error stop
    r8 = -3.9d0
    x = int(r8, 16)
    if (x /= -3) error stop
    x = 41
    r8 = x
    if (r8 /= 41.0d0) error stop
    via_kind = 7
    if (via_kind /= 7) error stop

    wide = 1
    do i = 1, 70
        wide = wide * 2
    end do
    if (wide <= 0) error stop
    if (add16(wide, 3_16) /= wide + 3) error stop

    h = huge(x)
    built = 1
    do i = 1, 126
        built = built * 2
    end do
    built = built + (built - 1)
    if (h /= built) error stop
    if (h <= wide) error stop
    print *, h

    x = -42
    write(s, '(i0)') x
    if (trim(adjustl(s)) /= '-42') error stop
    write(s, '(i5)') x
    if (s(1:5) /= '  -42') error stop
    write(s, '(i0)') h
    if (len_trim(adjustl(s)) /= 39) error stop
    write(s, *) h
    if (index(s, '170141183460469231731687303715884105727') == 0) error stop

    if (mod_huge /= p_huge) error stop
    if (mod_tagged /= 2_16 ** 62) error stop
    if (int(4611686018427387904_8, 16) /= 2_16 ** 62) error stop
    if (mod_arr(1) /= p_huge .or. mod_arr(2) /= -p70) error stop
    if (neg_arr(1) /= -p_huge .or. neg_arr(2) /= p70) error stop

    if (mod(p_huge, 10_16) /= 7_16) error stop
    if (modulo(-p_huge, 10_16) /= 3_16) error stop
    if (abs(-p_huge) /= p_huge) error stop
    if (sign(p70, -1_16) /= -p70) error stop
    if (ishft(1_16, 70) /= p70) error stop
    if (ibset(0_16, 70) /= p70) error stop
    if (ibclr(p70 + 3_16, 70) /= 3_16) error stop
    if (ibits(p70 + 7_16, 68, 4) /= 4_16) error stop
    if (.not. btest(p70, 70)) error stop
    if (popcnt(p70 + 3_16) /= 3 .or. poppar(p70 + 3_16) /= 1) error stop
    if (leadz(p70) /= 57 .or. trailz(p70) /= 70) error stop
    if (leadz(0_16) /= 128 .or. trailz(0_16) /= 128) error stop
    if (maskr(70, 16) /= p70 - 1_16 .or. maskl(65, 16) /= shiftl(-1_16, 63)) error stop
    if (dshiftl(1_16, p_huge, 70) /= p70 + ishft(p_huge, -58)) error stop
    if (dshiftr(15_16, 0_16, 126) /= 60_16) error stop
    if (ishftc(3_16, 70, 128) /= ishft(3_16, 70)) error stop
    if (merge_bits(3_16, p70, maskr(64, 16)) /= p70 + 3_16) error stop
    if (.not. bgt(-1_16, 1_16) .or. .not. blt(1_16, -1_16)) error stop
    if (transfer(p_huge, 0_16) /= p_huge) error stop

    x = p70 + 7_16
    y = 10_16
    i4 = 0; s70 = 70; s68 = 68; s4 = 4
    if (mod(x, y) /= 1_16 .or. modulo(-x, y) /= 9_16) error stop
    if (ishft(1_16, s70) /= p70 .or. .not. btest(x, s70)) error stop
    if (ibits(x, s68, s4) /= 4_16 .or. maskr(s70, 16) /= p70 - 1_16) error stop
    if (ishftc(3_16, s70, 128) /= ishft(3_16, 70)) error stop
    if (popcnt(p70 + 3_16 + i4 * 0) /= 3) error stop
    if (leadz(x - 7_16) /= 57 .or. trailz(x - 7_16) /= 70) error stop
end program

integer(16) function add16(a, b) bind(c)
    integer(16), value :: a, b
    add16 = a + b
end function add16
