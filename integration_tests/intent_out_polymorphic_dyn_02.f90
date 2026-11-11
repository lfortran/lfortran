module intent_out_polymorphic_dyn_02_m
   implicit none
   type :: core
      character(len=10) :: fixed = 'abc'
      character(len=1) :: sep = '.'
      character(len=:), allocatable :: msg
      integer :: k = 1
   contains
      procedure :: destroy
   end type
   type :: holder
      type(core) :: c
      character(len=4) :: tag = 'tag'
   end type
contains
   subroutine destroy(me)
      class(core), intent(out) :: me
   end subroutine
end module

program intent_out_polymorphic_dyn_02
   use intent_out_polymorphic_dyn_02_m
   implicit none
   type(core) :: a
   type(holder) :: h

   ! The components that are not allocatable stay valid when the dummy
   ! argument is reset, and the later deallocation does not free them again.
   a%msg = "hello"
   a%k = 5
   call a%destroy()
   if (allocated(a%msg)) error stop 1
   if (a%fixed /= 'abc' .or. a%sep /= '.' .or. a%k /= 1) error stop 2
   a%fixed = 'xyz'
   call a%destroy()
   if (a%fixed /= 'abc') error stop 3

   h%c%msg = "world"
   call h%c%destroy()
   call h%c%destroy()
   if (allocated(h%c%msg)) error stop 4
   if (h%c%fixed /= 'abc' .or. h%tag /= 'tag') error stop 5
end program
