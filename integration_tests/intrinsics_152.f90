program intrinsics_152
    integer :: k1= 5
    integer :: k2= 5_4
    integer :: k3= 58_8
    integer :: k4= 8
    integer :: k5 = 9
    integer :: k6 = 2

    integer, parameter :: sp = selected_real_kind(6,37)
    integer, parameter :: dp = selected_real_kind(15,307)

    print*, sp
    if (sp /= 4) error stop
    print*, dp
    if (dp /= 8) error stop

    print*, selected_real_kind()
    if (selected_real_kind() /= 4) error stop
    print*, selected_real_kind(1, 5)
    if (selected_real_kind(1, 5) /= 4) error stop
    print*, selected_real_kind(3, 32)
    if (selected_real_kind(3, 32) /= 4) error stop
    print*, selected_real_kind(5, 39)
    if (selected_real_kind(5, 39) /= 8) error stop
    print*, selected_real_kind(6, 51)
    if (selected_real_kind(6, 51) /= 8) error stop
    print*, selected_real_kind(7, 2)
    if (selected_real_kind(7, 2) /= 8) error stop
    print*, selected_real_kind(9, 1)
    if (selected_real_kind(9, 1) /= 8) error stop
    print*, selected_real_kind(12, 33)
    if (selected_real_kind(12, 33) /= 8) error stop
    print*, selected_real_kind(14, 32)
    if (selected_real_kind(14, 32) /= 8) error stop
    print*, selected_real_kind(7, 38, 2)
    if (selected_real_kind(7, 38, 2) /= 8) error stop
    print*, selected_real_kind(8, 38, 3)
    if (selected_real_kind(8, 38, 3) /= -5) error stop
    print*, selected_real_kind(9, 38, 2)
    if (selected_real_kind(9, 39, 2) /= 8) error stop
    print*, selected_real_kind(16, 37) ! output form lfortran is 8 whereas gfortran is 10
    print*, selected_real_kind(17, 37) ! output form lfortran is 8 whereas gfortran is 10

    print*, selected_real_kind(k1)
    if (selected_real_kind(k1) /= 4) error stop
    print*, selected_real_kind(k2)
    if (selected_real_kind(k2) /= 4) error stop
    print*, selected_real_kind(k1, k3)
    if (selected_real_kind(k1, k3) /= 8) error stop
    print*, selected_real_kind(k5, k4)
    if (selected_real_kind(k5, k4) /= 8) error stop
    print*, selected_real_kind(k1, k5)
    if (selected_real_kind(k1, k5) /= 4) error stop
    print*, selected_real_kind(k5, k2, k6)
    if (selected_real_kind(k5, k2, k6) /= 8) error stop
    print*, selected_real_kind(k5, k2, 3)
    if (selected_real_kind(k5, k2, 3) /= -5) error stop
    print*, selected_real_kind(34)
    if (selected_real_kind(34) /= -1) error stop
    print*, selected_real_kind(50,10,2)
    if (selected_real_kind(50,10,2) /= -1) error stop
    print*, selected_real_kind(45,16)
    if (selected_real_kind(50,10,2) /= -1) error stop

end
