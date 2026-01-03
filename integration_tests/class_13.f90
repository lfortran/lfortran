module class_13_mod
   type :: abstract_type
   contains
      procedure, nopass :: integer_method, integer_method_function
   end type abstract_type
contains

   subroutine integer_method(n)
      integer, intent(out) :: n
      n = 10
   end subroutine integer_method

   function integer_method_function() result(r)
      integer :: r
      r = 12
   end function integer_method_function
end module class_13_mod

program class_13
   use class_13_mod
   implicit none
   type(abstract_type) :: obj
   integer :: n
   n = 11
   call obj%integer_method(n)
   print *, n
   if (n /= 10) error stop
   n = obj%integer_method_function()
   print *, n
   if (n /= 12) error stop
end program
