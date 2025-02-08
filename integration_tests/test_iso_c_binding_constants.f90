program test_iso_c_binding_constants
   use iso_c_binding

   print *, c_int8_t
   if (c_int8_t /= 1) error stop

   print *, c_int16_t
   if (c_int16_t /= 2) error stop

   print *, c_int32_t
   if (c_int32_t /= 4) error stop

   print *, c_int64_t
   if (c_int64_t /= 8) error stop

   print *, c_int
   if (c_int /= 4) error stop

   print *, c_short
   if (c_short /= 2) error stop

   print *, c_long
   if (c_long /= 8) error stop

   print *, c_long_long
   if (c_long_long /= 8) error stop

   print *, c_size_t
   if (c_size_t /= 8) error stop

   print *, c_float
   if (c_float /= 4) error stop

   print *, c_double
   if (c_double /= 8) error stop

   ! Currently unsupported
   ! print *, c_long_double
   ! if (c_long_double /= 10) error stop

   print *, c_float_complex
   if (c_float_complex /= 4) error stop

   print *, c_double_complex
   if (c_double_complex /= 8) error stop

   ! Currently unsupported
   ! print *, c_long_double_complex
   ! if (c_long_double_complex /= 10) error stop

   print *, c_bool
   if (c_bool /= 1) error stop

   print *, c_char
   if (c_char /= 1) error stop

   print *, c_null_char
   if (c_null_char /= char(0)) error stop
   
end program test_iso_c_binding_constants
