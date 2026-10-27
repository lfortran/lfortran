! Compiled before the submodules that implement the procedures it calls.
program submodule_58
    use submodule_58_m
    implicit none
    if (get_value() /= 101) error stop 1
    call bump(3)
    if (counter /= 7) error stop 2
    if (label /= "bumped") error stop 3
    if (get_label() /= "nnnn-s2") error stop 4
    print *, "ok"
end program
