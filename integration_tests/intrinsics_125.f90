program intrinsic_125
    implicit none

    real, allocatable :: array(:)
    integer :: freq(100), i, rand_nums_gen_limit, rand_num, min_rand_num, max_rand_num
    real :: expected_freq, accuracy_min, accuracy_max

    rand_nums_gen_limit = 5000000
    expected_freq = float(rand_nums_gen_limit) / 100

    freq = 0

    allocate (array(rand_nums_gen_limit))

    call random_number(array)

    do i = 1, rand_nums_gen_limit
        rand_num = 1 + mod(int(array(i) * 100), 100)
        freq(rand_num) = freq(rand_num) + 1
    end do

    min_rand_num = minval(freq)
    max_rand_num = maxval(freq)

    accuracy_min = min_rand_num / expected_freq
    accuracy_max = max_rand_num / expected_freq

    print *, accuracy_min, accuracy_max
    if ((accuracy_max - accuracy_min) > 0.05) error stop

end program
