program submodule_52_main
    use submodule_52_mod
    implicit none
    integer :: r

    r = string_len_dim("hello")
    if (r /= 5) error stop
    print *, r

    r = string_len_dim("ab")
    if (r /= 2) error stop
    print *, r

    r = string_len_dim("submodule_test")
    if (r /= 14) error stop
    print *, r
end program
