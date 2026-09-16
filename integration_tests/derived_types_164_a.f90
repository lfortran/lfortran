module derived_types_164_a
    implicit none
    integer, parameter :: base = 4
    type :: t
        integer :: i = 0
        integer :: j = 1
    end type
    type(t), parameter :: z = t(7, 8)
    type(t), parameter :: zk = t(base)
    type(t), target, save :: tgt = t(5, 6)
end module
