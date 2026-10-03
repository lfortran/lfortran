! Pointers whose initial targets are parts of another module's variable.
module global_init_16_b
    use global_init_16_a, only: h
    implicit none
    character(len=3), pointer :: ps => h%s
    integer, pointer :: pv => h%v(2)
end module global_init_16_b
