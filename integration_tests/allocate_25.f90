program allocate_25
   implicit none
   integer, allocatable :: x
   allocate(x, source = 5)
   print *, x
   if (x /= 5) error stop
end program allocate_25
