! Compiled before the submodules (#14167).
program submodule_63
    use submodule_63_m
    implicit none
    if (s2_value() /= 2*10 + 1 + 14) error stop 1
    if (s1_value() /= 14) error stop 2
    if (s2_value() /= 2*10 + 2 + 14) error stop 3
    print *, "ok"
end program
