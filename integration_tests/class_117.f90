! Test same_type_as with class(*)
program class_117
   implicit none
   type :: t1
      integer :: x
   end type t1

   type :: t2
      real :: y
   end type t2

   class(*), allocatable :: a, b, c

   a = t1(1)
   b = t1(2)
   c = t2(3.0)

   if (.not. same_type_as(a, b)) error stop
   if (same_type_as(a, c)) error stop

   print *, "PASS"
end program class_117
