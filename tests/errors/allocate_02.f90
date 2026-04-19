program allocate_66
   implicit none
   type :: T
      integer :: x = 0
   end type T
   class(T), allocatable :: obj
   allocate(obj, source = get_return())
contains
   function get_return() result(r)
      class(T), allocatable :: r
   end function get_return
end program allocate_66
