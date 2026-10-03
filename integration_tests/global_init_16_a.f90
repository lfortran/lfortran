! The storage global_init_16_b's pointers are initially associated with.
module global_init_16_a
    implicit none
    type :: holder
        character(len=3) :: s = "abc"
        integer :: v(3) = [1, 2, 3]
    end type
    type(holder), target :: h
end module global_init_16_a
