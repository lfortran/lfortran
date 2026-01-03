module present_01_module
implicit none

contains

   subroutine foo1(a)
      real, intent(in), optional :: a(:)
      call foo2(a)
   end subroutine

   subroutine foo2(a)
      real, intent(in), optional :: a(..)
      if (present(a)) error stop
   end subroutine

end module

program present_01
use present_01_module
implicit none

   call foo1()

end
