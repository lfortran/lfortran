program enum_01
    implicit none
    enum, bind(c)
        enumerator :: red, blue
        enumerator yellow
    end enum

    integer, parameter :: compiler_enum = kind(red)

    if (red /= 0) error stop
    if (blue /= 1) error stop
    if (yellow /= 2) error stop

end program enum_01
