! The storage test_init_show_llvm_b's pointers are initially associated with.
module test_init_show_llvm_a
    implicit none
    type :: holder
        character(len=3) :: s = "abc"
        integer :: v(3) = [1, 2, 3]
    end type
    type(holder), target :: h
end module test_init_show_llvm_a
