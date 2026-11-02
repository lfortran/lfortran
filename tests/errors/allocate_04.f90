program allocate_04
   implicit none
   type :: T
      integer :: x = 5
   end type T
   type :: holder_t
      type(T), allocatable :: comp
   end type holder_t
   type(holder_t) :: h
   type(T), allocatable :: obj
   allocate(obj, source = h%comp)
   print *, "SHOULD NOT REACH"
end program allocate_04
