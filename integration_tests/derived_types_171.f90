program derived_types_171
implicit none

type :: t
    real(8) :: ra(2)
end type

type(t), parameter :: p = t([1.5d0, 2.5d0])
type(t), parameter :: q(0:1) = [t([3.5d0, 4.5d0]), t([5.5d0, 6.5d0])]
real(8), parameter :: q_last = q(1)%ra(2)
real(8) :: y(2)

y = p%ra
if (any(y /= [1.5d0, 2.5d0])) error stop 1
y = q(1)%ra
if (any(y /= [5.5d0, 6.5d0])) error stop 2
if (q_last /= 6.5d0) error stop 3
end program derived_types_171
