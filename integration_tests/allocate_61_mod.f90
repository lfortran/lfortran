module allocate_61_mymod
   implicit none
   type :: t
      integer :: x = 42
   end type
   type :: w
      type(t), allocatable :: a
   end type
contains
   function make_t() result(res)
      type(t) :: res
      res%x = 10
   end function
end module
