program complex_22
   implicit none
   complex, pointer :: ptr_complex4
   complex(8), pointer :: ptr_complex8
   complex, target :: complex4_1
   complex(8), target :: complex8_1
   complex8_1 = (1, 2)
   complex4_1 = (1, 2)
   ptr_complex8 => complex8_1
   ptr_complex4 => complex4_1
   print *, cmplx(ptr_complex4, kind=8)
   if (ptr_complex8 /= cmplx(ptr_complex4, kind=8)) error stop
end program complex_22