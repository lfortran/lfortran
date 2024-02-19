program associate_13
    implicit none

    complex(8), target :: my_complex(2, 3)
    complex(8), pointer :: my_complex_mirror(:, :)
    real(8), pointer :: my_complex_re(:, :)
    real(8), pointer :: my_complex_im(:, :)

    my_complex = reshape([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], [2, 3])

    my_complex_re => my_complex%re
    print *, my_complex_re(1, 1), my_complex_re(1, 2), my_complex_re(1, 3)
    print *, my_complex_re(2, 1), my_complex_re(2, 2), my_complex_re(2, 3)
    if (abs(my_complex_re(1, 1) - 1) > 1e-8_8) error stop
    if (abs(my_complex_re(1, 2) - 5) > 1e-8_8) error stop
    if (abs(my_complex_re(1, 3) - 9) > 1e-8_8) error stop
    if (abs(my_complex_re(2, 1) - 3) > 1e-8_8) error stop
    if (abs(my_complex_re(2, 2) - 7) > 1e-8_8) error stop
    if (abs(my_complex_re(2, 3) - 11) > 1e-8_8) error stop

    my_complex_im => my_complex%im
    print *, my_complex_im(1, 1), my_complex_im(1, 2), my_complex_im(1, 3)
    print *, my_complex_im(2, 1), my_complex_im(2, 2), my_complex_im(2, 3)
    if (abs(my_complex_im(1, 1) - 2) > 1e-8_8) error stop
    if (abs(my_complex_im(1, 2) - 6) > 1e-8_8) error stop
    if (abs(my_complex_im(1, 3) - 10) > 1e-8_8) error stop
    if (abs(my_complex_im(2, 1) - 4) > 1e-8_8) error stop
    if (abs(my_complex_im(2, 2) - 8) > 1e-8_8) error stop
    if (abs(my_complex_im(2, 3) - 12) > 1e-8_8) error stop

end program
