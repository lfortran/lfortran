module derived_type_c_procedure_01_m
contains
subroutine s(r)
  integer, intent(out) :: r
  type :: p
    integer :: x
    real :: y
  end type
  type(p) :: v
  v%x = 3
  v%y = 2.5
  r = v%x
end subroutine
end module
program derived_type_c_procedure_01
use derived_type_c_procedure_01_m
integer :: r
call s(r)
if (r /= 3) error stop
end program
