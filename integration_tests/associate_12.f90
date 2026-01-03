program associate_12
    implicit none

    complex, target :: my_complex_one_dim(2)
    complex, pointer :: my_complex_one_dim_mirror(:)
    real, pointer :: my_complex_one_dim_re(:)
    real, pointer :: my_complex_one_dim_im(:)

    complex, target :: my_complex(2, 3)
    complex, pointer :: my_complex_mirror(:, :)
    real, pointer :: my_complex_re(:, :)
    real, pointer :: my_complex_im(:, :)

    my_complex_one_dim(1) = (2, 5)
    my_complex_one_dim(2) = (-2, -5)

    my_complex_one_dim_mirror => my_complex_one_dim
    print *, my_complex_one_dim_mirror(1), my_complex_one_dim_mirror(2)
    if (abs(my_complex_one_dim_mirror(1) - (2, 5)) > 1e-5) error stop
    if (abs(my_complex_one_dim_mirror(2) - (-2, -5)) > 1e-5) error stop

    my_complex_one_dim_re => my_complex_one_dim%re
    print *, my_complex_one_dim_re(1), my_complex_one_dim_re(2)
    if (abs(my_complex_one_dim_re(1) - 2) > 1e-5) error stop
    if (abs(my_complex_one_dim_re(2) - (-2)) > 1e-5) error stop

    my_complex_one_dim_im => my_complex_one_dim%im
    print *, my_complex_one_dim_im(1), my_complex_one_dim_im(2)
    if (abs(my_complex_one_dim_im(1) - 5) > 1e-5) error stop
    if (abs(my_complex_one_dim_im(2) - (-5)) > 1e-5) error stop

    my_complex = reshape([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], [2, 3])

    my_complex_mirror => my_complex
    print *, my_complex_mirror(1, 1), my_complex_mirror(1, 2), my_complex_mirror(1, 3)
    print *, my_complex_mirror(2, 1), my_complex_mirror(2, 2), my_complex_mirror(2, 3)
    if (abs(my_complex_mirror(1, 1) - (1, 2)) > 1e-5) error stop
    if (abs(my_complex_mirror(1, 2) - (5, 6)) > 1e-5) error stop
    if (abs(my_complex_mirror(1, 3) - (9, 10)) > 1e-5) error stop
    if (abs(my_complex_mirror(2, 1) - (3, 4)) > 1e-5) error stop
    if (abs(my_complex_mirror(2, 2) - (7, 8)) > 1e-5) error stop
    if (abs(my_complex_mirror(2, 3) - (11, 12)) > 1e-5) error stop


    my_complex_re => my_complex%re
    print *, my_complex_re(1, 1), my_complex_re(1, 2), my_complex_re(1, 3)
    print *, my_complex_re(2, 1), my_complex_re(2, 2), my_complex_re(2, 3)
    if (abs(my_complex_re(1, 1) - 1) > 1e-5) error stop
    if (abs(my_complex_re(1, 2) - 5) > 1e-5) error stop
    if (abs(my_complex_re(1, 3) - 9) > 1e-5) error stop
    if (abs(my_complex_re(2, 1) - 3) > 1e-5) error stop
    if (abs(my_complex_re(2, 2) - 7) > 1e-5) error stop
    if (abs(my_complex_re(2, 3) - 11) > 1e-5) error stop

    my_complex_im => my_complex%im
    print *, my_complex_im(1, 1), my_complex_im(1, 2), my_complex_im(1, 3)
    print *, my_complex_im(2, 1), my_complex_im(2, 2), my_complex_im(2, 3)
    if (abs(my_complex_im(1, 1) - 2) > 1e-5) error stop
    if (abs(my_complex_im(1, 2) - 6) > 1e-5) error stop
    if (abs(my_complex_im(1, 3) - 10) > 1e-5) error stop
    if (abs(my_complex_im(2, 1) - 4) > 1e-5) error stop
    if (abs(my_complex_im(2, 2) - 8) > 1e-5) error stop
    if (abs(my_complex_im(2, 3) - 12) > 1e-5) error stop

end program
