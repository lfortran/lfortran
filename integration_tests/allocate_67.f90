program allocate_67
   implicit none
   type :: T
      integer :: x = 5
   end type T
   type(T), allocatable :: src, obj
   allocate(src)
   allocate(obj, source = src)
   if (obj%x /= 5) error stop
   print *, "ok"
end program allocate_67
