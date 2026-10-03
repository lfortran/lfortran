program real128_selected_real_kind_01
    implicit none
    integer, parameter :: qp = selected_real_kind(30)
    integer, parameter :: qp2 = selected_real_kind(33, 4000)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer :: p, r
    real(qp) :: x

    if (qp /= 16) error stop 1
    if (qp2 /= 16) error stop 2
    if (dp /= 8) error stop 3
    if (selected_real_kind(34) /= -1) error stop 4
    if (selected_real_kind(30, 5000) /= -2) error stop 11
    if (selected_real_kind(34, 5000) /= -3) error stop 12
    if (selected_real_kind(33, 4931, 3) /= -5) error stop 13

    ! runtime path
    p = 30
    r = 0
    if (selected_real_kind(p, r) /= 16) error stop 5
    p = 33
    r = 4931
    if (selected_real_kind(p, r) /= 16) error stop 6
    p = 15
    r = 307
    if (selected_real_kind(p, r) /= 8) error stop 7
    p = 34
    if (selected_real_kind(p) /= -1) error stop 8
    p = 30
    r = 5000
    if (selected_real_kind(p, r) /= -2) error stop 14
    p = 34
    if (selected_real_kind(p, r) /= -3) error stop 15

    x = 1.0_qp / 3.0_qp
    if (kind(x) /= 16) error stop 9
    if (abs(x * 3.0_qp - 1.0_qp) > 1.0e-30_qp) error stop 10
    print *, qp, dp, x
end program
