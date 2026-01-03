module class_31_test_module

   type :: Composed
      integer :: x
   end type Composed

   type :: Wrapper
      class(Composed), allocatable :: obj
   end type Wrapper

contains

   function init(obj) result(self)
      type(Wrapper) :: self
      class(Composed), intent(in) :: obj

      ! needed because lfortran does not automatically allocate this
      allocate(self%obj)

      self%obj = obj
   end function init

end module class_31_test_module

program class_31
   use class_31_test_module
   implicit none

   class(Composed), allocatable :: c
   type(Wrapper) :: t

   allocate(c)
   c%x = 42

   t = init(c)

   print *, "value of t%obj%x: ", t%obj%x
   if (t%obj%x /= 42) error stop

   c%x = 3
   ! verify deep copy
   print *, "value of t%obj%x: ", t%obj%x
   if (t%obj%x == 3) error stop

end program class_31