program intrinsics_140
    implicit none
    integer, parameter :: wp = selected_int_kind(9)
    integer, parameter :: dp = selected_int_kind(18_8)
    integer :: k1 = 1
    integer :: k2 = 2
    integer :: k3 = 3
    integer :: k4 = 4
    integer(8) :: k5 = 5
    integer(8) :: k6 = 6
    integer(8) :: k7 = 7
    integer(8) :: k8 = 8
    integer(8) :: k9 = 9
    integer(8) :: k10 = 10
    integer(8) :: k11 = 11
    
    print *, wp
    if (wp /= 4) error stop
    print *, dp
    if (dp /= 8) error stop
    
    print *, selected_int_kind(1)
    if (selected_int_kind(1) /= 1) error stop
    print *, selected_int_kind(2)
    if (selected_int_kind(2) /= 1) error stop
    print *, selected_int_kind(3)
    if (selected_int_kind(3) /= 2) error stop
    print *, selected_int_kind(4)
    if (selected_int_kind(4) /= 2) error stop
    print *, selected_int_kind(5)
    if (selected_int_kind(5) /= 4) error stop
    print *, selected_int_kind(6)
    if (selected_int_kind(6) /= 4) error stop
    print *, selected_int_kind(7)
    if (selected_int_kind(7) /= 4) error stop
    print *, selected_int_kind(8)
    if (selected_int_kind(8) /= 4) error stop
    print *, selected_int_kind(9)
    if (selected_int_kind(9) /= 4) error stop
    print *, selected_int_kind(10)
    if (selected_int_kind(10) /= 8) error stop
    print *, selected_int_kind(11)
    if (selected_int_kind(11) /= 8) error stop
    
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

    print *, kind(selected_int_kind(1))
    if (kind(selected_int_kind(1)) /= 4) error stop
    print *, kind(selected_int_kind(2_8))
    if (kind(selected_int_kind(2_8)) /= 4) error stop
end program