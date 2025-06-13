module class_52_mod

   type, abstract :: abstract_type
   contains
      procedure(nelements), deferred :: nelements
   end type abstract_type

   abstract interface
      pure function nelements(self) result(n)
         import;
         implicit none
         class(abstract_type), intent(in) :: self
         integer                          :: n
      end function nelements
   end interface

   type, extends(abstract_type) :: concrete_type
   contains
      procedure :: nelements => concrete_nelements
   end type concrete_type

   type :: Wrapper
      class(concrete_type), allocatable :: obj
   contains
      procedure :: caller_struct_member, caller_simple_subrout, caller_simple_func
   end type Wrapper

contains

   subroutine caller_struct_member(self)
      class(Wrapper), intent(in) :: self
      real(8), dimension(self%obj%nelements()) :: a

      a = [1.0, 2.0, 3.0, 4.0, 5.0]
      print *, a

      print *, size(a)
      if (size(a) /= 5) error stop
   end subroutine

   subroutine caller_simple_subrout(self)
      class(Wrapper), intent(in) :: self
      real(8), dimension(concrete_nelements(self%obj)) :: a

      print *, size(a)
      if (size(a) /= 5) error stop
   end subroutine

   integer function caller_simple_func(self)
      class(Wrapper), intent(in) :: self
      real(8), dimension(concrete_nelements(self%obj)) :: a

      caller_simple_func = size(a)
   end function

   pure function concrete_nelements(self) result(n)
      class(concrete_type), intent(in) :: self
      integer :: n
      n = 5
   end function concrete_nelements

end module class_52_mod

program class_52
   use class_52_mod
   implicit none

   type(Wrapper) :: w
   type(concrete_type), allocatable :: c
   integer :: i

   allocate(c)
   allocate(concrete_type :: w%obj)

   call w%caller_struct_member()
   call w%caller_simple_subrout()

   i = w%caller_simple_func()
   print *, i
   if (i /= 5) error stop

end program