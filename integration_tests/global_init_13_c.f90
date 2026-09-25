! A module global_init_13_a uses targets from; see global_init_13.f90.
module global_init_13_c
    implicit none
    integer, target :: c_tgt = 30
    integer, target :: c_arr(3) = [31, 32, 33]
contains
    integer function c_twice(x)
        integer, intent(in) :: x
        c_twice = 2*x
    end function c_twice
end module global_init_13_c
