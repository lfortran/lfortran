module class_36_module

   type :: SomeType
      integer :: x = 2
   contains
      procedure, pass   :: subroutine_pass
      procedure, nopass :: subroutine_nopass

      procedure, pass :: function_pass
      procedure, nopass :: function_nopass
   end type SomeType

contains

   subroutine subroutine_pass(self, var)
      class(SomeType), intent(in)  :: self
      real(8), intent(out) :: var

      print *, "var inside subroutine_pass: ", var
      if (var /= 16.33) error stop

      var = 32.33
      call self%subroutine_nopass(var)
   end subroutine subroutine_pass

   subroutine subroutine_nopass(var)
      real(8), intent(out) :: var

      print *, "var inside subroutine_nopass: ", var
      if (var /= 32.33) error stop

      var = 64.33
   end subroutine subroutine_nopass

   integer function function_pass (self)
      class(SomeType) :: self
      function_pass = self%x
   end function function_pass

   integer function function_nopass (var)
      integer :: var
      function_nopass = var
   end function function_nopass

end module class_36_module

program class_36
    use class_36_module
    implicit none

    type(SomeType) :: my_var
    real(8) :: r

    r = 16.33

    call my_var%subroutine_pass(r)

    print *, "r after subroutine call: ", r
    if (r /= 64.33) error stop

    print *, "return from function_pass: ",  function_pass(my_var)
    print *, "return from function_nopass: ", function_nopass(42)
end program