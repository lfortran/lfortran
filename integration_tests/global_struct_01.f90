module global_struct_module

   type :: myType
      logical :: a = .true.
      integer :: i = 2
      integer :: j = 3
      integer :: k
      character(len=10) :: c = "Hello"
      integer :: array(3) = [1, 2, 3]
      character(len=1) :: c_array(3) = ['a', 'b', 'c']
   end type myType

   type(myType) :: globalType

contains
   subroutine config(self)
      class(myType), intent(inout) :: self
      print *, "a = ", self%a
      if (self%a .neqv. .true.) error stop

      print *, "i = ", self%i
      if (self%i /= 2) error stop

      print *, "j = ", self%j
      if (self%j /= 3) error stop

      print *, "k = ", self%k
      if (self%k /= 0) error stop

      self%k = self%i + self%j
      print *, "k = ", self%k
      if (self%k /= 5) error stop

      print *, "c = ", self%c
      if (self%c /= "Hello") error stop

      print *, "array = ", self%array
      if (any(self%array /= [1, 2, 3])) error stop

      print *, "c_array = ", self%c_array
      if (any(self%c_array /= ['a', 'b', 'c'])) error stop

   end subroutine config

end module global_struct_module

program main
   use global_struct_module

   call config(globalType)
end program main
