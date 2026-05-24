program traits_04
   implicit none
   integer :: initial
   integer :: implements
   integer :: sealed

   initial = 1
   implements = 2
   sealed = 3

   if (initial /= 1) error stop 1
   if (implements /= 2) error stop 2
   if (sealed /= 3) error stop 3
   print *, "PASS"
end program traits_04
