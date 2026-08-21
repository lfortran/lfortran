program array_section_oob_02
   ! Writing through an out-of-bounds array section must abort at runtime
   ! instead of silently corrupting memory outside the array.
   implicit none
   real :: x(3)
   integer :: i
   x = 1.0
   i = 0
   x(i:i+1) = 9.0
   print *, x
end program array_section_oob_02
