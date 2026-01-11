module procedure_25_module
   implicit none
contains
   subroutine run(f)
      implicit none
      interface
         integer function f()
         end function f
      end interface

      procedure(f), pointer :: func_ptr
      integer :: x

      func_ptr => f
      x = func_ptr()
      print *, x
      if(x /= 42) error stop
   end subroutine run
end module


program procedure_25
   use procedure_25_module
   implicit none
   call run(test_func)
contains
   integer function test_func()
      test_func = 42
   end function test_func

end program