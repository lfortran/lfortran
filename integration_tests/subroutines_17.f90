program subroutines_17
   implicit none

   call recursive_fun
   
contains

   subroutine chrosen(x)
      real, intent(in) :: x(:)
      print *, "Hello from chrosen!", x
   end subroutine chrosen

   subroutine OBJ(x)
      real, intent(in) :: x(:)
   end subroutine OBJ

   subroutine lincoa(calfun)
      procedure(OBJ) :: calfun
      call calfun([1.0])
   end subroutine lincoa

   subroutine recursive_fun
      call lincoa(chrosen)
   end subroutine recursive_fun

end program