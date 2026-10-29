module parameter_30_m
implicit none
integer, parameter :: atomic_logical_kind = 8
end module parameter_30_m

program parameter_30
use parameter_30_m, only: atomic_logical_kind
implicit none
print *, atomic_logical_kind
if (atomic_logical_kind /= 8) error stop
end program parameter_30
