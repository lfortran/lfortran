program format_44
    ! Test formatting of special floating-point values (Infinity, NaN)
    implicit none
    real :: zero, pos_inf, neg_inf, nan_val
    character(20) :: str

    zero = 0.0
    pos_inf = 1.0 / zero
    neg_inf = -1.0 / zero
    nan_val = zero / zero

    ! Test G format with Infinity
    write(str, '(G12.5)') pos_inf
    if (trim(adjustl(str)) /= "Infinity") error stop "G12.5 +Inf failed"

    write(str, '(G12.5)') neg_inf
    if (trim(adjustl(str)) /= "-Infinity") error stop "G12.5 -Inf failed"

    ! Test E format with Infinity
    write(str, '(E12.5)') pos_inf
    if (trim(adjustl(str)) /= "Infinity") error stop "E12.5 +Inf failed"

    write(str, '(E12.5)') neg_inf
    if (trim(adjustl(str)) /= "-Infinity") error stop "E12.5 -Inf failed"

    ! Test ES format with Infinity
    write(str, '(ES12.5)') pos_inf
    if (trim(adjustl(str)) /= "Infinity") error stop "ES12.5 +Inf failed"

    ! Test EN format with Infinity
    write(str, '(EN12.5)') pos_inf
    if (trim(adjustl(str)) /= "Infinity") error stop "EN12.5 +Inf failed"

    ! Test G format with NaN
    write(str, '(G12.5)') nan_val
    if (trim(adjustl(str)) /= "NaN") error stop "G12.5 NaN failed"

    ! Test E format with NaN
    write(str, '(E12.5)') nan_val
    if (trim(adjustl(str)) /= "NaN") error stop "E12.5 NaN failed"

    print *, "PASS"
end program
