module class_46_mod

   type :: SomeType
   contains
      procedure :: method1
      procedure :: method2
   end type SomeType

contains

   subroutine method1(self, arr)
      class(SomeType), intent(in) :: self
      integer, intent(in) :: arr(:)
      integer, allocatable :: local(:)

      local = self%method2(arr)
      print *, "local: ", local

      if (.not. all(local == [1, 2, 3])) error stop
   end subroutine method1

   function method2(self, a) result(out)
      class(SomeType), intent(in) :: self
      integer, intent(in) :: a(:)
      integer :: out(size(a))

      out = a
   end function method2

end module class_46_mod

program class_46
   use class_46_mod

   type(SomeType) :: obj
   integer :: arr(3) = [1, 2, 3]

   call obj%method1(arr)
end program class_46
