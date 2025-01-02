module my_module

   type :: my_super_type
      integer :: x
   end type


   type :: my_type
      integer :: x
      type(my_super_type) :: super
      ! procedure(print_something), pointer :: p_ptr
   contains
      final :: finalize
      procedure :: proc => print_something
      procedure :: unary, plus
      generic :: operator(+) => unary, plus
      generic :: g_proc => plus, unary
   end type my_type

contains

   subroutine finalize(self)
      type(my_type), intent(inout) :: self
   end subroutine finalize


   subroutine print_something(self, str)
      class(my_type), intent(inout) :: self
      character(len=*), intent(in) :: str
      print *, str
   end subroutine print_something


   subroutine say_something(self, msg)
      class(my_type), intent(inout) :: self
      character(len=*), intent(in) :: msg
      print *, msg
   end subroutine


   pure type(my_type) function unary(self, x)
      class(my_type), intent(in) :: self, x
   end function unary


   pure type(my_type) function plus(self)
      class(my_type), intent(in) :: self
   end function plus

end module my_module