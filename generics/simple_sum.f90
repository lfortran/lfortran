module simple_generics
  use, intrinsic :: iso_fortran_env, only: real64

  abstract interface :: INumeric
    integer(kind=8) | real(kind=8)
  end interface INumeric

  contains
  
    function simple_sum{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T) :: s
      integer :: i
      s = T(0)
      do i = 1, size(x)
         s = s + x(i)
      end do
    end function simple_sum

end module simple_generics
