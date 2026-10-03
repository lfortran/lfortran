program do_concurrent_14
    implicit none
    integer :: a(10, 10)
    integer :: j_test, k_test

    do concurrent (integer :: j = 1:10, k = 1:10)
        a(j, k) = j + k
    end do

    do j_test = 1, 10
        do k_test = 1, 10
            if (a(j_test, k_test) /= j_test + k_test) error stop
        end do
    end do
end program do_concurrent_14
