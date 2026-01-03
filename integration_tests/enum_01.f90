program enum_01
    implicit none
    enum, bind(c)
        enumerator :: red, blue = 3
        enumerator yellow
    end enum

    enum, bind(c)
        enumerator :: green = 10
        enumerator purple
    end enum

    integer, parameter :: compiler_enum = kind(red)

    if (red /= 0) error stop
    if (blue /= 3) error stop
    if (yellow /= 4) error stop
    if (green /= 10) error stop
    if (purple /= 11) error stop

end program enum_01
