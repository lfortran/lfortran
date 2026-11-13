program read_123
    ! List-directed internal read from a character array (#17312).
    ! Each element is one record. Later values continue in that record,
    ! and a record with only blanks left is not read again.
    implicit none
    character(len=20) :: buf_same(1)
    character(len=10) :: buf_mult(2)
    character(len=8) :: buf_real(2)
    character(len=3) :: buf_short(1)
    character(len=2) :: exact(2)
    character(len=5) :: split(2)
    character(len=4) :: blanked(3)
    character(len=6) :: comma_pad(2)
    character(len=5) :: comma_one(1)
    character(len=3) :: buf_null(2)
    character(len=6) :: buf_comma(2)
    character(len=10) :: scalar
    integer :: a, b, c, d, e, f
    real :: x, y

    buf_same(1) = '10 20'
    a = -1
    b = -1
    read(buf_same, *) a, b
    if (a /= 10 .or. b /= 20) error stop 1

    buf_mult(1) = '30'
    buf_mult(2) = '40'
    c = -1
    d = -1
    read(buf_mult, *) c, d
    if (c /= 30 .or. d /= 40) error stop 2

    buf_real(1) = '1.25'
    buf_real(2) = '3.5'
    x = -1.0
    y = -1.0
    read(buf_real, *) x, y
    if (abs(x - 1.25) > 1.0e-6 .or. abs(y - 3.5) > 1.0e-6) error stop 3

    buf_short(1) = '1 2'
    a = -1
    b = -1
    read(buf_short, *) a, b
    if (a /= 1 .or. b /= 2) error stop 4

    exact(1) = '30'
    exact(2) = '40'
    e = -1
    f = -1
    read(exact, *) e, f
    if (e /= 30 .or. f /= 40) error stop 5

    split(1) = '1 2'
    split(2) = '3'
    a = -1
    b = -1
    c = -1
    read(split, *) a, b, c
    if (a /= 1 .or. b /= 2 .or. c /= 3) error stop 6

    blanked(1) = '10'
    blanked(2) = ' '
    blanked(3) = '20'
    a = -1
    b = -1
    read(blanked, *) a, b
    if (a /= 10 .or. b /= 20) error stop 7

    comma_one(1) = '10,20'
    a = -1
    b = -1
    read(comma_one, *) a, b
    if (a /= 10 .or. b /= 20) error stop 8

    comma_pad(1) = '10,'
    comma_pad(2) = '20'
    a = -1
    b = -1
    read(comma_pad, *) a, b
    if (a /= 10 .or. b /= 20) error stop 9

    scalar = '10 20'
    a = -1
    b = -1
    read(scalar, *) a, b
    if (a /= 10 .or. b /= 20) error stop 10

    ! A comma that ends one record and another that opens the next are
    ! two separators, so the value between them is null.
    buf_null(1) = '10,'
    buf_null(2) = ',20'
    a = 7
    b = 8
    c = 9
    read(buf_null, *) a, b, c
    if (a /= 10 .or. b /= 8 .or. c /= 20) error stop 11

    ! DECIMAL='comma' uses ';' as the separator. '1,5' is 1.5.
    buf_comma(1) = '1,5;'
    buf_comma(2) = '2,5'
    x = -1.0
    y = -1.0
    read(buf_comma, *, decimal='comma') x, y
    if (abs(x - 1.5) > 1.0e-6 .or. abs(y - 2.5) > 1.0e-6) error stop 12
end program
