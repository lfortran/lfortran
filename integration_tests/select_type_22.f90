module m
   implicit none

   type :: t
   contains
      procedure :: init
   end type

contains

   subroutine init(self)
      class(t), intent(inout) :: self
      print *, "init"
   end subroutine

end module


program p
   use m
   implicit none

   class(t), allocatable :: x
   logical :: called

   called = .false.
   allocate(t :: x)

   select type(y => x)
    class default
      call y%init()
      called = .true.
   end select

   if (.not. called) error stop

end program
