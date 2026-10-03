program format_65
    implicit none
    real :: zero = 0.0, pos_inf, neg_inf, nan_val
    character(len=9) :: buf

    pos_inf = 1.0/zero
    neg_inf = -1.0/zero
    nan_val = 0.0/zero

    ! F format: Infinity fits in width 9
    write(buf, "(F9.0)") pos_inf
    if (adjustl(buf) /= "Infinity") error stop
    write(buf, "(F9.0)") neg_inf
    if (buf /= "-Infinity") error stop
    write(buf, "(F9.0)") nan_val
    if (adjustl(buf) /= "NaN") error stop

    print *, "PASS"
end program format_65
