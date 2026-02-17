module select_type_28_mod
   implicit none
   private

   type, abstract :: base
   end type base

   type, extends(base) :: mid
   end type mid

   type, extends(base) :: child
   end type child

   public :: wrapper, child

   type :: wrapper
      class(base), allocatable :: obj
   contains
      procedure :: run
   end type wrapper

contains

   module subroutine run(self, other)
      class(wrapper), intent(inout) :: self
      class(wrapper), intent(in)    :: other

      logical :: entered
      entered = .false.

      select type(a => self%obj)
       type is(child)

         select type(b => other%obj)
          type is(child)
            entered = .true.
          class default
            error stop
         end select

       class default
         error stop
      end select

      if (.not. entered) then
         error stop
      end if

   end subroutine run

end module select_type_28_mod


program select_type_28
   use select_type_28_mod
   implicit none

   type(wrapper) :: w1, w2

   ! Allocate both as child
   allocate(child :: w1%obj)
   allocate(child :: w2%obj)

   call w1%run(w2)

end program select_type_28
