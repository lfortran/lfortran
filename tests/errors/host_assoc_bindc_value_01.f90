! An internal procedure uses a value dummy argument of its bind(c) host
module host_assoc_bindc_value_01_m
use iso_c_binding, only: c_double
implicit none
contains
subroutine h(n) bind(c)
complex(c_double), value :: n
integer :: r
call inner()
print *, r
contains
subroutine inner()
r = int(real(n))
end subroutine
end subroutine
end module
