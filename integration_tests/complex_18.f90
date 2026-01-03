program complex_18
    implicit none

    complex(4), target :: my_complex1(2, 3)
    complex(8), target :: my_complex2(2, 3)
    integer :: i, j, k

    k = 1
    do i = 1, 2
        do j = 1, 3
            my_complex1(i, j) = complex(k, k + 1)
            my_complex2(i, j) = complex(k, k + 1)
            k = k + 2
        end do
    end do

    print *, my_complex1

    if (abs(my_complex1(1, 1) - (1, 2)) > 1e-5) error stop
    if (abs(my_complex1(1, 2) - (3, 4)) > 1e-5) error stop
    if (abs(my_complex1(1, 3) - (5, 6)) > 1e-5) error stop
    if (abs(my_complex1(2, 1) - (7, 8)) > 1e-5) error stop
    if (abs(my_complex1(2, 2) - (9, 10)) > 1e-5) error stop
    if (abs(my_complex1(2, 3) - (11, 12)) > 1e-5) error stop

    print *, my_complex2

    if (abs(my_complex2(1, 1) - (1, 2)) > 1e-8_8) error stop
    if (abs(my_complex2(1, 2) - (3, 4)) > 1e-8_8) error stop
    if (abs(my_complex2(1, 3) - (5, 6)) > 1e-8_8) error stop
    if (abs(my_complex2(2, 1) - (7, 8)) > 1e-8_8) error stop
    if (abs(my_complex2(2, 2) - (9, 10)) > 1e-8_8) error stop
    if (abs(my_complex2(2, 3) - (11, 12)) > 1e-8_8) error stop
end program
