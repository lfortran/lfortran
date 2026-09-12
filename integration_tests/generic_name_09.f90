module generic_name_09_mod
 implicit none
 interface addCNullChar
  module procedure addCNullChar
 end interface
contains
 function addCNullChar(x) result(y)
  integer, intent(in) :: x
  integer :: y
  y = x + 1
 end function addCNullChar
end module generic_name_09_mod

program generic_name_09
 use generic_name_09_mod
 implicit none
 integer :: r
 r = addCNullChar(3)
 print *, r
 if (r /= 4) error stop
 if (addCNullChar(10) /= 11) error stop
 print *, "PASS"
end program generic_name_09
