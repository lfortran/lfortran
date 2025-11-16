program read_06
    implicit none
    call check_value("10", 10)
    call check_value("3 7", 3)
contains
    subroutine check_value(src, expected)
        character(len=*), intent(in) :: src
        integer, intent(in) :: expected
        integer :: value, ios

        read(src, *, iostat=ios) value
        if (ios /= 0) error stop
        if (value /= expected) error stop
    end subroutine check_value
end program read_06
