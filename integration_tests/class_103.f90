! Test for https://github.com/lfortran/lfortran/issues/2925
! Typed allocation of polymorphic arrays with data members
! MRE from issue comment by difference-scheme (the data member variant)
module class_103_m

   type :: Base
      real :: data = 2.0
   end type Base

   type, extends(Base) :: Extended
   end type Extended

contains

   subroutine allocator(array)
      class(Base), allocatable, intent(out) :: array(:)
      allocate( Extended :: array(1) )
   end subroutine allocator

end module class_103_m

program class_103
   use class_103_m
   implicit none
   class(Base), allocatable :: arr(:)

   call allocator(arr)

   if (.not. allocated(arr)) error stop
   if (size(arr) /= 1) error stop
   if (abs(arr(1)%data - 2.0) > 1e-6) error stop
   print *, "PASS"
end program class_103
