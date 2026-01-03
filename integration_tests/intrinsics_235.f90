program intrinsics_235
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    ! real(sp) :: x
    real(sp) :: y, z
    real(dp) :: a, b, c

    ! x = 971.72820_sp 
    y = 23.82920_sp
    z = 62.739713_sp

    a = 9271.72826260_dp
    b = 23.82926260_dp
    c = 62.73971326260_dp

    ! print *, lgamma(x)
    ! if (abs(lgamma(x) - 5710.34375) > 1e-5) error stop

    print *, lgamma(y)
    if (abs(lgamma(y) - 5.10680733e+01) > 1e-5) error stop

    print *, lgamma(23.82920)
    if (abs(lgamma(23.82920) - 5.10680733e+01) > 1e-5) error stop

    ! print *, lgamma(z)
    ! if (abs(lgamma(z) - 1.95790390e+02) > 1e-5) error stop

    print *, lgamma(62.739713)
    if (abs(lgamma(62.739713) - 1.95790390e+02) > 1e-5) error stop

    print *, lgamma(a)
    if (abs(lgamma(a) - 7.54193119924598432e+04_dp) > 1e-12) error stop

    print *, lgamma(9271.72826260_dp)
    if (abs(lgamma(9271.72826260_dp) - 7.54193119924598432e+04_dp) > 1e-12) error stop

    print *, lgamma(b)
    if (abs(lgamma(b) - 5.10682662991165941e+01_dp) > 1e-12) error stop

    print *, lgamma(23.82926260_dp)
    if (abs(lgamma(23.82926260_dp) - 5.10682662991165941e+01_dp) > 1e-12) error stop

    print *, lgamma(c)
    if (abs(lgamma(c) - 1.95790392620801725e+02_dp) > 1e-12) error stop

    print *, lgamma(62.73971326260_dp)
    if (abs(lgamma(62.73971326260_dp) - 1.95790392620801725e+02_dp) > 1e-12) error stop

    print *, algama(y)
    if (abs(algama(y) - 5.10680733e+01) > 1e-5) error stop

    print *, algama(23.82920)
    if (abs(algama(23.82920) - 5.10680733e+01) > 1e-5) error stop

    ! print *, algama(z)
    ! if (abs(algama(z) - 1.95790390e+02) > 1e-5) error stop

    print *, algama(62.739713)
    if (abs(algama(62.739713) - 1.95790390e+02) > 1e-5) error stop

    print *, algama(a)
    if (abs(algama(a) - 7.54193119924598432e+04_dp) > 1e-12) error stop

    print *, algama(9271.72826260_dp)
    if (abs(algama(9271.72826260_dp) - 7.54193119924598432e+04_dp) > 1e-12) error stop

    print *, algama(b)
    if (abs(algama(b) - 5.10682662991165941e+01_dp) > 1e-12) error stop

    print *, algama(23.82926260_dp)
    if (abs(algama(23.82926260_dp) - 5.10682662991165941e+01_dp) > 1e-12) error stop

    print *, algama(c)
    if (abs(algama(c) - 1.95790392620801725e+02_dp) > 1e-12) error stop

    print *, algama(62.73971326260_dp)
    if (abs(algama(62.73971326260_dp) - 1.95790392620801725e+02_dp) > 1e-12) error stop

    print *, dlgama(a)
    if (abs(dlgama(a) - 7.54193119924598432e+04_dp) > 1e-12) error stop

    print *, dlgama(9271.72826260_dp)
    if (abs(dlgama(9271.72826260_dp) - 7.54193119924598432e+04_dp) > 1e-12) error stop

    print *, dlgama(b)
    if (abs(dlgama(b) - 5.10682662991165941e+01_dp) > 1e-12) error stop

    print *, dlgama(23.82926260_dp)
    if (abs(dlgama(23.82926260_dp) - 5.10682662991165941e+01_dp) > 1e-12) error stop

    print *, dlgama(c)
    if (abs(dlgama(c) - 1.95790392620801725e+02_dp) > 1e-12) error stop

    print *, dlgama(62.73971326260_dp)
    if (abs(dlgama(62.73971326260_dp) - 1.95790392620801725e+02_dp) > 1e-12) error stop

end program