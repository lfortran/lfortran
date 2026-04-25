program implicit_interface_35
    procedure(), pointer :: ifunc => null()
    procedure(), pointer :: xfunc => null()

    ifunc => itriple
    if (ifunc(4) /= 12) error stop
    print *, ifunc(4)

    xfunc => xhalf
    if (abs(xfunc(6.0) - 3.0) > 1.0e-6) error stop
    print *, xfunc(6.0)

contains
    function itriple(n)
        integer, intent(in) :: n
        integer :: itriple
        itriple = n * 3
    end function

    function xhalf(x)
        real, intent(in) :: x
        real :: xhalf
        xhalf = x / 2.0
    end function
end program
