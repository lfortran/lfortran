subroutine pass_array_by_data_20_check(x)
    integer, intent(in) :: x(:)
    if (size(x) /= 2) error stop
    if (x(2) /= 7) error stop
end subroutine

integer function pass_array_by_data_20_second(x)
    integer, intent(in) :: x(:)
    pass_array_by_data_20_second = x(2)
end function

program pass_array_by_data_20
    implicit none
    interface
        subroutine pass_array_by_data_20_check(x)
            integer, intent(in) :: x(:)
        end subroutine
        integer function pass_array_by_data_20_second(x)
            integer, intent(in) :: x(:)
        end function
    end interface
    integer :: v(4)
    v = [3, 5, 7, 9]
    call pass_array_by_data_20_check(v(1:4:2))
    if (pass_array_by_data_20_second(v(2:3)) /= 7) error stop
    print *, "OK"
end program
