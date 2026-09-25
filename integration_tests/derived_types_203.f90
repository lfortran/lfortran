program derived_types_203
implicit none
type :: outer
   integer :: e(0)      ! zero-size component
   integer :: h = 0
end type
type(outer) :: o
o%h = 111
o = outer(e=5, h=7)
if (o%h /= 7) error stop
end program derived_types_203