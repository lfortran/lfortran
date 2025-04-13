module simple_generics
  use, intrinsic :: iso_fortran_env, only: real64

  abstract interface
  end interface


  contains
  
    function simple_sum(x) result(s)
      integer, intent(in) :: x(:)
      integer :: s
      integer :: i
      s = 0
      do i = 1, size(x)
         s = s + i
      end do
    end function simple_sum

end module simple_generics
