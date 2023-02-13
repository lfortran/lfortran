module enum_02_module
    implicit none
    enum, bind(c)
        enumerator :: red, blue
        enumerator yellow
    end enum

    integer, parameter :: compiler_enum = kind(red)

end module enum_02_module
