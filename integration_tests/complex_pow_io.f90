program complex_pow_io
    implicit none
    complex :: z32, base32, res32
    complex(8) :: z64, base64, res64

    z32 = (3.0, 4.0)
    if (z32**2 /= (-7.0, 24.0)) error stop 1

    z64 = (3.0d0, 4.0d0)
    if (z64**2 /= (-7.0d0, 24.0d0)) error stop 2

    base32 = (0.0, 0.0)
    res32 = base32 ** 0.0
    res32 = base32 ** (-2.0)

    base64 = (0.0d0, 0.0d0)
    res64 = base64 ** 0.0d0
    res64 = base64 ** (-2.0d0)

    if (real(res32) == 999.0) print *, "dummy"

    write(*, "(a)", advance="no") "A"
    write(*, "(a)", advance="no") " "
    write(*, "(a)") "B"
end program