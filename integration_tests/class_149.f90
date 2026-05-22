module class_149_mod
   implicit none
   type :: base_t
      integer :: i = 1
   end type
   type, extends(base_t) :: child_t
      real :: r = 2.0
   end type
contains
   subroutine inner_check(input, expected_i)
      class(*), intent(in) :: input
      integer, intent(in) :: expected_i
      select type(input)
      class is (base_t)
         if (input%i /= expected_i) error stop 1
         print *, "got i=", input%i
      class default
         error stop 2
      end select
   end subroutine

   subroutine outer(activation_in, expected_i)
      class(*), optional, intent(in) :: activation_in
      integer, intent(in) :: expected_i
      if (present(activation_in)) then
         call inner_check(activation_in, expected_i)
      end if
   end subroutine
end module

program class_149
   use class_149_mod
   implicit none
   class(base_t), allocatable :: a_alloc
   class(base_t), pointer :: a_ptr
   type(base_t) :: a_concrete
   type(child_t) :: c_concrete

   ! 1. type(base_t) -> class(*) directly
   a_concrete%i = 42
   call inner_check(a_concrete, 42)

   ! 2. type(child_t) -> class(*) directly
   c_concrete%i = 11
   c_concrete%r = 3.5
   call inner_check(c_concrete, 11)

   ! 3. class(base_t), allocatable -> class(*) (via outer)
   allocate(a_alloc)
   a_alloc%i = 99
   call outer(a_alloc, 99)

   ! 4. class(base_t), pointer -> class(*) (via outer)
   allocate(a_ptr)
   a_ptr%i = 77
   call outer(a_ptr, 77)
   deallocate(a_ptr)

   print *, "PASS"
end program
