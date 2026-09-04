program array_section_oob_01
   ! Reading an array section below the lower bound must abort at runtime.
   implicit none
   real :: x(3)
   integer :: i
   x = 1.0
   i = 0
   print *, x(i:i+1)
end program array_section_oob_01
