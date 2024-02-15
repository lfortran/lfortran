program intrinsics_140

    integer :: k1 = 1
    integer :: k2 = 2
    integer :: k3 = 3
    integer :: k4 = 4
    integer :: k5 = 5
    integer :: k6 = 6
    integer :: k7 = 7
    integer :: k8 = 8
    integer :: k9 = 9
    integer :: k10 = 10
    integer :: k11 = 11

    print *, selected_int_kind(k1)
    if (selected_int_kind(k1) /= 1) error stop
    print *, selected_int_kind(k2)
    if (selected_int_kind(k2) /= 1) error stop
    print *, selected_int_kind(k3)
    if (selected_int_kind(k3) /= 2) error stop
    print *, selected_int_kind(k4)
    if (selected_int_kind(k4) /= 2) error stop
    print *, selected_int_kind(k5)
    if (selected_int_kind(k5) /= 4) error stop
    print *, selected_int_kind(k6)
    if (selected_int_kind(k6) /= 4) error stop
    print *, selected_int_kind(k7)
    if (selected_int_kind(k7) /= 4) error stop
    print *, selected_int_kind(k8)
    if (selected_int_kind(k8) /= 4) error stop
    print *, selected_int_kind(k9)
    if (selected_int_kind(k9) /= 4) error stop
    print *, selected_int_kind(k10)
    if (selected_int_kind(k10) /= 8) error stop
    print *, selected_int_kind(k11)
    if (selected_int_kind(k11) /= 8) error stop

end