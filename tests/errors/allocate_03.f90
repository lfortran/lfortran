program allocate_03
   implicit none
   type :: T
      integer :: x = 5
   end type T
   type(T), allocatable :: src, obj
   allocate(obj, source = src)
   print *, "SHOULD NOT REACH"
end program allocate_03
