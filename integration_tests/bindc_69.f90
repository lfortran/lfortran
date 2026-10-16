module bindc_69_mod
    use iso_c_binding, only: c_char
    implicit none
    character(kind=c_char), bind(C, name="bindc_69_init") :: b(3) = ['x', 'y', 'z']
    character(kind=c_char), bind(C, name="bindc_69_grid") :: g(3,2)
    character(kind=c_char), bind(C, name="bindc_69_buf") :: buf(5)
end module

program bindc_69
    use iso_c_binding, only: c_char, c_int
    use bindc_69_mod, only: b, g, buf
    implicit none
    interface
        integer(c_int) function bindc_69_check() bind(C)
            import :: c_int
        end function
        subroutine bindc_69_fill() bind(C)
        end subroutine
    end interface
    integer :: i, j

    if (any(b /= ['x', 'y', 'z'])) error stop
    do j = 1, 2
        do i = 1, 3
            g(i,j) = achar(iachar('A') + (i-1) + 3*(j-1))
        end do
    end do
    buf = ['1', '2', '3', '4', '5']
    if (bindc_69_check() /= 0) error stop

    call bindc_69_fill()
    if (any(b /= ['x', 'Y', 'z'])) error stop
    do j = 1, 2
        do i = 1, 3
            if (g(i,j) /= achar(iachar('a') + (i-1) + 3*(j-1))) error stop
        end do
    end do
    if (any(buf /= ['h', 'e', 'l', 'l', 'o'])) error stop
    print *, b, g, buf
end program
