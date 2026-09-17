program derived_types_171
implicit none

type :: t
    real(8) :: ra(2)
end type

type(t), parameter :: p = t([1.5d0, 2.5d0])
real(8) :: y(2)

y = p%ra
if (any(y /= [1.5d0, 2.5d0])) error stop 1
end program derived_types_171
