program array_section_oob_03
   ! A strided section of an allocatable array that runs outside the
   ! bounds must abort at runtime.
   implicit none
   real, allocatable :: x(:)
   integer :: i
   allocate(x(3))
   x = 1.0
   i = 0
   print *, x(i:i+4:2)
end program array_section_oob_03
