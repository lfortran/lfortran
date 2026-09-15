module derived_types_166_a
    implicit none
    type :: inner
        integer :: k = 0
    end type
    type :: t
        integer :: i = 0
        type(inner) :: n
    end type
    type(inner), parameter :: zin = inner(3)
    type(t), parameter :: z = t(7, zin)
end module
