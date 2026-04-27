module separate_compilation_49a_mod
   implicit none
   type :: t
      integer :: i
      integer, allocatable :: a(:)
   end type
   type(t), allocatable :: d(:)
end module

subroutine separate_compilation_49_sub()
   use separate_compilation_49a_mod
   implicit none
   allocate(d(2))
   if (allocated(d(1)%a)) error stop "d(1)%a unexpectedly allocated"
   if (allocated(d(2)%a)) error stop "d(2)%a unexpectedly allocated"
   allocate(d(1)%a(1))
   allocate(d(2)%a(1))
   d(1)%a(1) = 11
   d(2)%a(1) = 22
   if (d(1)%a(1) /= 11) error stop "wrong value in d(1)%a"
   if (d(2)%a(1) /= 22) error stop "wrong value in d(2)%a"
   deallocate(d(1)%a)
   deallocate(d(2)%a)
   deallocate(d)
end subroutine
