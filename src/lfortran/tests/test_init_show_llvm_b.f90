! Pointers whose initial targets are parts of another module's variable.
module test_init_show_llvm_b
    use test_init_show_llvm_a, only: h
    implicit none
    character(len=3), pointer :: ps => h%s
    integer, pointer :: pv => h%v(2)
end module test_init_show_llvm_b
