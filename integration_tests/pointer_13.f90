program pointer_13
    integer, target :: i
    pointer :: iptr
    procedure(), pointer :: ifptr

    i = 10

    iptr => i
    ifptr => iprint

    if (iptr /= 10) error stop
    if (ifptr() /= 100) error stop

    print *, iptr
    print *, ifptr()

contains

    function iprint()
       iprint = 100
    end function iprint

end program pointer_13
