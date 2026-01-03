program intrinsics_80
    implicit none
    integer :: x(2)
    x = 10

    print *, func(x)
    if (x(1) /= 10) error stop
    if (x(2) /= 10) error stop

    contains
        integer function func(R) result(i)
            integer, intent(in) :: R(:)
            i = sum(R)
        end function
end program
