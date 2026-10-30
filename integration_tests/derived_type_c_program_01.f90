program derived_type_c_program_01
type :: p
  integer :: x
  real :: y
end type
type(p) :: v
v%x = 3
v%y = 2.5
if (v%x /= 3) error stop
if (abs(v%y - 2.5) > 1e-6) error stop
end program
