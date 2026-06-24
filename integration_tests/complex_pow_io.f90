program complex_pow_io
    implicit none
    complex :: base32, res32
    complex(8) :: base64, res64

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