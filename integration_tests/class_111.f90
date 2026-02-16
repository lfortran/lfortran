module class_111_mod

   type :: MyType
      procedure(factory), pointer, nopass :: val => null()
   contains
      procedure :: get
   end type MyType

   abstract interface
      subroutine factory()
      end subroutine factory
   end interface

contains

   recursive subroutine get(self, key, val)
      class(MyType),               intent(in)  :: self
      character(*),                intent(in)  :: key
      procedure(factory), pointer, intent(out) :: val
      val => self%val
   end subroutine get

end module class_111_mod

program class_111

   use class_111_mod

   character(*),       parameter :: key = 'test'
   procedure(factory), pointer   :: val

   type(MyType) :: obj

   obj%val => my_factory
   call obj%get(key, val)

   if (.not. associated(val)) error stop
   if (.not. associated(val, my_factory)) error stop

   print *, "PASS"

contains

   subroutine my_factory()
   end subroutine my_factory

end program class_111
