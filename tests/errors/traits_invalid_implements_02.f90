module traits_invalid_implements_02
   implicit none

   abstract interface :: IValue
      integer function get_value(self)
         import :: IValue
         class(IValue), intent(in) :: self
      end function get_value
   end interface IValue

   type, implements(IValue) :: BadValue
   contains
      procedure :: get_value => get_value_real
   end type BadValue

contains

   real function get_value_real(self) result(value)
      class(BadValue), intent(in) :: self
      value = 1.0
   end function get_value_real

end module traits_invalid_implements_02
