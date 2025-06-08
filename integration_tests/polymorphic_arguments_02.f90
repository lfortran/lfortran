
program polymorphic_argument_02
   implicit none
   integer :: i_value = 1
   real :: r_value = 1
   character(len=1) :: c_value = "c"
   logical :: l_value = .true.

   interface str
      procedure str_scalar
   end interface str

   ! Calling generic procedure with any argument
   print *, str(i_value)
   if (str(i_value) /= 0) error stop

   print *, str(r_value)
   if (str(r_value) /= 0) error stop

   print *, str(c_value)
   if (str(c_value) /= 0) error stop

   print *, str(l_value)
   if (str(l_value) /= 0) error stop

   ! Calling subroutine with polymorphic parameter, with any argument
   print *, str_scalar(i_value)
   if (str_scalar(i_value) /= 0) error stop

   print *, str_scalar(r_value)
   if (str_scalar(r_value) /= 0) error stop

   print *, str_scalar(c_value)
   if (str_scalar(c_value) /= 0) error stop

   print *, str_scalar(l_value)
   if (str_scalar(l_value) /= 0) error stop

CONTAINS

   function str_scalar(g1)
      class(*), intent(in)  :: g1
      integer :: str_scalar
      str_scalar = 0
   end function str_scalar

end program polymorphic_argument_02
