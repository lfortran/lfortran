program read_real16_long_token_01
    ! A long but valid numeric token (up to 99 characters) must be read in full
    ! for real(16) and complex(16).
    implicit none
    integer, parameter :: qp = 16
    real(qp) :: x, xs(2)
    complex(qp) :: z, zs(2)
    integer :: u
    open(newunit=u, status="scratch", form="formatted")
    write(u, "(a)") "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.25"
    write(u, "(a)") "(00000000000000000000000000000000000000000000000000000000000000000000001.5,-00000000000000000000000000000000000000000000000000000000000000000000002.5)"
    write(u, "(a)") "00000000000000000000000000000000000000000000000000000000000000000000000000000000000003.0 00000000000000000000000000000000000000000000000000000000000000000000000000000000000004.0"
    write(u, "(a)") "(0000000000000000000000000000000000000000000000000000000000001.0,0000000000000000000000000000000000000000000000000000000000002.0) (0000000000000000000000000000000000000000000000000000000000003.0,0000000000000000000000000000000000000000000000000000000000004.0)"
    rewind(u)
    read(u, *) x
    if (abs(x - 0.25_qp) > 1.0e-30_qp) error stop "real(16) scalar"
    read(u, *) z
    if (abs(real(z) - 1.5_qp) > 1.0e-30_qp) error stop "complex(16) re"
    if (abs(aimag(z) + 2.5_qp) > 1.0e-30_qp) error stop "complex(16) im"
    read(u, *) xs
    if (abs(xs(1) - 3.0_qp) > 1.0e-30_qp) error stop "real(16) array 1"
    if (abs(xs(2) - 4.0_qp) > 1.0e-30_qp) error stop "real(16) array 2"
    read(u, *) zs
    if (abs(real(zs(1)) - 1.0_qp) > 1.0e-30_qp) error stop "complex(16) array 1"
    if (abs(aimag(zs(2)) - 4.0_qp) > 1.0e-30_qp) error stop "complex(16) array 2"
    close(u)
end program
