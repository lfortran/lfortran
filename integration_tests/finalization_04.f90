module finalization_04_mod
   implicit none
   type :: item_type
      character(len=:), allocatable :: name
   contains
      final :: destroy_item
   end type item_type
contains
   function new_item(name) result(self)
      character(len=*), intent(in) :: name
      type(item_type) :: self
      self%name = name
   end function new_item
   subroutine destroy_item(self)
      type(item_type), intent(inout) :: self
      if (allocated(self%name)) deallocate(self%name)
   end subroutine destroy_item
end module finalization_04_mod

program finalization_04
   use finalization_04_mod
   implicit none
   type(item_type) :: items(2)
   items = [new_item("hello"), new_item("world")]
   if (items(1)%name /= "hello") error stop
   if (items(2)%name /= "world") error stop
   print *, "ok"
end program finalization_04
