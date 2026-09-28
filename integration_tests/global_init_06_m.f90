! A chain of modules. Each declares a pointer association, which is a link
! time constant and is laid out as the pointer's own static initializer, and
! an array of a derived type with a character member, which is not and so has
! to run as an executable statement: that is what gives each module a startup
! initializer of its own.
module global_init_06_base
    implicit none

    type :: tagged
        integer :: h = 0
        character(len=3) :: tag = "bad"
    end type

    integer, target :: base_target = 3
    integer, pointer :: base_ptr => base_target
    type(tagged) :: base_arr(2) = tagged(3, "bbb")
end module

module global_init_06_mid
    use global_init_06_base
    implicit none
    integer, target :: mid_target = 2
    integer, pointer :: mid_ptr => mid_target
    type(tagged) :: mid_arr(2) = tagged(2, "mmm")
end module

module global_init_06_top
    use global_init_06_mid
    implicit none
    integer, target :: top_target = 1
    integer, pointer :: top_ptr => top_target
    type(tagged) :: top_arr(2) = tagged(1, "ttt")
end module
