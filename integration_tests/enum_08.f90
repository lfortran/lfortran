program enum_08
    implicit none

    enum, bind(c)
        enumerator :: OPTION_A = 1
        enumerator :: OPTION_B = 2
        enumerator :: OPTION_C = 4
        enumerator :: OPTION_D = 8
        enumerator :: OPTION_E = 16
    end enum

    enum, bind(c)
        enumerator :: STATUS_OK, STATUS_WARN, STATUS_ERR
    end enum

    enum, bind(c)
        enumerator :: COLOR_RED = 5
        enumerator :: COLOR_GREEN
        enumerator :: COLOR_BLUE
    end enum

    if (OPTION_A /= 1) error stop
    if (OPTION_B /= 2) error stop
    if (OPTION_C /= 4) error stop
    if (OPTION_D /= 8) error stop
    if (OPTION_E /= 16) error stop

    if (STATUS_OK /= 0) error stop
    if (STATUS_WARN /= 1) error stop
    if (STATUS_ERR /= 2) error stop

    if (COLOR_RED /= 5) error stop
    if (COLOR_GREEN /= 6) error stop
    if (COLOR_BLUE /= 7) error stop

end program enum_08
