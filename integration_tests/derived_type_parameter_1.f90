program derived_type_parameter_1
  implicit none
  character(len=1), parameter :: letters(4) = ['a', 'b', 'c', 'd']
  type calendar
     character(len=1) :: chars(4) = letters
  end type calendar
  type(calendar), parameter :: calen = calendar()
  
  print *, calen%chars
end program derived_type_parameter_1
