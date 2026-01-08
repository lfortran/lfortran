module jstringbuffer
  implicit none

  type :: StringBuffer
    integer, private :: size = 0
  contains
    procedure :: number_of_elems
  end type StringBuffer

contains

  function number_of_elems(self) result(result)
    class(StringBuffer), intent(in) :: self
    integer :: result
    result = self%size
  end function

end module jstringbuffer

program main
  print *, "OK"
end program
