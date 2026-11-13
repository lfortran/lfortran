program integer_kind_16
    implicit none
    integer, parameter :: k19 = selected_int_kind(19)
    integer, parameter :: k38 = selected_int_kind(38)
    integer, parameter :: k39 = selected_int_kind(39)
    integer(16) :: x, y, h, built, wide
    integer(k19) :: via_kind
    integer(8) :: i8
    integer(4) :: i4
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
end program

integer(16) function add16(a, b) bind(c)
    integer(16), value :: a, b
    add16 = a + b
end function add16
