! An internal procedure uses a value dummy argument of its bind(c) host
module host_assoc_bindc_value_01_m
use iso_c_binding, only: c_int
implicit none
contains
subroutine h(n) bind(c)
integer(c_int), value :: n
integer :: r
call inner()
print *, r
contains
subroutine inner()
r = n + 1
end subroutine
end subroutine
end module
