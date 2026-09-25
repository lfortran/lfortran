! Module variables of global_init_13_a's type, in a module of their own, and
! entities with the names of two targets; see global_init_13.f90.
module global_init_13_b
    use global_init_13_a, only: t
    implicit none
    integer, target :: arr(3) = [91, 92, 93]
    type(t) :: obj
    type(t) :: objs(2)
contains
    integer function c_twice(x)
        integer, intent(in) :: x
        c_twice = -x
    end function c_twice
end module global_init_13_b
