module select_type_25_mod
   implicit none

   type :: toml_value
   end type toml_value

   type, extends(toml_value) :: toml_keyval
      integer :: x
   end type toml_keyval

contains

   recursive subroutine visit(val)
      class(toml_value), intent(inout) :: val
      select type(val)
      class is(toml_keyval)
         call visit_keyval(val)
      end select
   end subroutine visit


   subroutine visit_keyval(val)
      type(toml_keyval), intent(inout) :: val
      val%x = 100
   end subroutine visit_keyval

end module select_type_25_mod


program select_type_25
   use select_type_25_mod
   implicit none

   class(toml_value), pointer :: obj
   class(toml_value), allocatable :: obj2
   type(toml_keyval), target :: temp
   obj => temp

   select type(obj)
   class is(toml_keyval)
      call visit(obj)
      if (obj%x /= 100) error stop
      allocate(obj2, source=obj)
      select type(obj2)
      class is(toml_keyval)
         if (obj2%x /= 100) error stop
      class default
         error stop
      end select
   class default
      error stop
   end select

end program select_type_25
