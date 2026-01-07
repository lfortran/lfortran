module member_access_mod
  implicit none

  type :: string_buffer
    integer, private :: size = 0
    integer, private :: arr(1) = 0
  contains
    procedure :: number_of_elems
  end type string_buffer

contains

  function number_of_elems(self) result(result)
    class(string_buffer), intent(in) :: self
    integer :: result
    result = self%size
    result = result + self%arr(1)
  end function number_of_elems

end module member_access_mod
