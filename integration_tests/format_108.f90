program format_108
    ! Test B0.m format descriptor: zero width with minimum digits
    ! must zero-pad on the left to m digits (GH issue: b0.32 ignored m)
    implicit none
    character(50) :: str

    write(str, "(B0.8)") 5
    if (str /= "00000101") error stop "B0.8 failed"

    write(str, "(B0.32)") 15
    if (str /= "00000000000000000000000000001111") error stop "B0.32 failed"

    write(str, "(B0.32)") 0
    if (str /= "00000000000000000000000000000000") error stop "B0.32 zero failed"

    write(str, "(B0.32)") -1
    if (str /= "11111111111111111111111111111111") error stop "B0.32 -1 failed"

    ! m smaller than the number of digits: no padding
    write(str, "(B0.2)") 255
    if (str /= "11111111") error stop "B0.2 failed"

    print *, "PASSED"
end program format_108
