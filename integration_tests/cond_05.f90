program cond_05
   implicit none
   logical, pointer :: ptr_bool1, ptr_bool2
   logical, target :: bool_true, bool_false
   complex, pointer :: ptr_complex4
   complex(8), pointer :: ptr_complex8
   complex, target :: complex4_1, complex4_2
   complex(8), target :: complex8_1
   bool_true = .true.
   bool_false = .false.

   ptr_bool1 => bool_true
   if (ptr_bool1 .eqv. bool_false) error stop
   if (ptr_bool1 .neqv. bool_true) error stop
   if (.not. ptr_bool1 .eqv. ptr_bool1) error stop

   ptr_bool1 => bool_true
   ptr_bool2 => bool_true
   if (.not. ptr_bool1 .eqv. ptr_bool2) error stop

   complex4_1 = (1, 2)
   ptr_complex4 => complex4_1
   complex4_2 = (2, 3)
   complex8_1 = complex4_1

   if (ptr_complex4 == complex4_2) error stop
   if (ptr_complex4 /= complex8_1) error stop
   if (.not. ptr_complex4 == (complex4_2 - (1,1))) error stop
end program cond_05
