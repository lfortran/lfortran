program format_51
    ! Test binary (B) format descriptor
    implicit none
    integer :: i
    character(50) :: str

    ! Binary format tests
    print "(A,B32.32)", 'binary 2: ', 2
    print "(A,B8.8)", 'binary 255: ', 255
    print "(A,B16.16)", 'binary -1: ', -1
    print "(A,B0)", 'binary 42: ', 42
    print "(A,B10)", 'binary 7: ', 7
    print "(B5.5)", 15
    print "(B3.3)", 4

    ! Verify with write statements
    write(str, "(B32.32)") 2
    if (trim(adjustl(str)) /= "00000000000000000000000000000010") error stop "B32.32 failed"

    write(str, "(B8.8)") 255
    if (trim(adjustl(str)) /= "11111111") error stop "B8.8 failed"

    write(str, "(B0)") 42
    if (trim(adjustl(str)) /= "101010") error stop "B0 failed"

    write(str, "(B10)") 7
    if (trim(adjustl(str)) /= "111") error stop "B10 failed"

    write(str, "(B5.5)") 15
    if (trim(adjustl(str)) /= "01111") error stop "B5.5 failed"

    print *, "PASSED"
end program format_51
