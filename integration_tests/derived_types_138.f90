program derived_types_138
   implicit none
   type :: element
      integer :: data
   end type
   type :: variable
      type(element), allocatable :: n(:)
   end type
   type(variable), target :: v
   integer, pointer :: dd(:)
   integer :: total

   allocate(v%n(3))
   v%n%data = [10, 20, 30]

   dd => v%n%data
   if (size(dd) /= 3) error stop "wrong size"
   if (dd(1) /= 10) error stop "dd(1) wrong"
   if (dd(2) /= 20) error stop "dd(2) wrong"
   if (dd(3) /= 30) error stop "dd(3) wrong"

   total = sum(dd)
   if (total /= 60) error stop "wrong sum"

   associate(aa => v%n%data)
      if (size(aa) /= 3) error stop "associate size wrong"
      if (sum(aa) /= 60) error stop "associate sum wrong"
   end associate

   print *, "OK"
end program
