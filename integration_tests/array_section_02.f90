program expr2
    implicit none

    integer :: r(4), i, tmp
    r = 17
    r(:2) = func(r(:3))

    tmp = 2
    do i = 1, size(r)
        if (i > 2) tmp = 1
        if (r(i) /= tmp * 17) error stop
    end do

contains

    function func(input) result(output)
        integer, intent(inout) :: input(:)
        integer :: output(size(input)-1)
        output = 2*input(:2)
        if (size(output) /= 2) error stop
    end function func
end program
