submodule (stdlib_error_separate_compilation_20) f18estop_separate_compilation_20
implicit none

contains

    module procedure error_stop
        integer, parameter :: i = 3
        code = i
    end procedure

end submodule f18estop_separate_compilation_20