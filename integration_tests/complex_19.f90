program complex_19
    implicit none

    integer, parameter :: dp = kind(0.d0)
    complex(dp), target :: my_complex(2, 3)
    real(dp) :: k
    integer :: i, j

    k = 1.0_dp
    do i = 1, 2
        do j = 1, 3
            my_complex(i, j) = complex(k, k + 1.0_dp)
            k = k + 2.0_dp
        end do
    end do

    print *, my_complex

    if (abs(my_complex(1, 1) - (1.0_dp, 2.0_dp)) > 1e-8_dp) error stop
    if (abs(my_complex(1, 2) - (3.0_dp, 4.0_dp)) > 1e-8_dp) error stop
    if (abs(my_complex(1, 3) - (5.0_dp, 6.0_dp)) > 1e-8_dp) error stop
    if (abs(my_complex(2, 1) - (7.0_dp, 8.0_dp)) > 1e-8_dp) error stop
    if (abs(my_complex(2, 2) - (9.0_dp, 10.0_dp)) > 1e-8_dp) error stop
    if (abs(my_complex(2, 3) - (11.0_dp, 12.0_dp)) > 1e-8_dp) error stop
end program
