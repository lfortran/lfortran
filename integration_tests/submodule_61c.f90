! Compiled before the submodule, and does not use the module that only the
! submodule uses (#14167).
program submodule_61
    use submodule_61_m
    implicit none
    if (s_value() /= 11 + 15 + 100) error stop 1
    if (s_value() /= 12 + 15 + 200) error stop 2
    if (s_tag() /= "rrrrrr") error stop 3
    print *, "ok"
end program
