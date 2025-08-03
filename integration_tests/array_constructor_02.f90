program array_constructor_02
   implicit none

   type build_target_ptr

      type(build_target_t), pointer :: ptr => null()

   end type build_target_ptr

   type :: build_target_t
      type(build_target_ptr), allocatable :: dependencies(:)
   end type

   type(build_target_t) :: a, b
   call add_dependency(a, b)

contains
   subroutine add_dependency(target, dependency)
      type(build_target_t), intent(inout) :: target
      type(build_target_t) , intent(in), target :: dependency

      allocate(target%dependencies(2))

      target%dependencies = [target%dependencies, build_target_ptr(dependency)]

   end subroutine add_dependency
end program
