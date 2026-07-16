module generic_name_10_mod
 implicit none
 interface get_text
  integer function get_text()
  end function get_text
  integer function get_text_a(x)
   integer, intent(in) :: x
  end function get_text_a
 end interface
end module generic_name_10_mod

integer function get_text()
 get_text = 42
end function get_text

integer function get_text_a(x)
 integer, intent(in) :: x
 get_text_a = x + 1
end function get_text_a

program generic_name_10
 use generic_name_10_mod
 implicit none
 integer :: r
 r = get_text()
 print *, r
 if (r /= 42) error stop
 if (get_text() /= 42) error stop
 if (get_text(10) /= 11) error stop
 print *, "PASS"
end program generic_name_10
