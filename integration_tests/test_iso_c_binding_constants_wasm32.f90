! The kinds of iso_c_binding that follow the C ABI of wasm32 (ILP32): long,
! size_t, intptr_t and ptrdiff_t are 4 bytes, the explicitly 64-bit types 8.
program test_iso_c_binding_constants_wasm32
   use iso_c_binding

   print *, c_long
   if (c_long /= 4) error stop

   print *, c_size_t
   if (c_size_t /= 4) error stop

   print *, c_intptr_t
   if (c_intptr_t /= 4) error stop

   print *, c_ptrdiff_t
   if (c_ptrdiff_t /= 4) error stop

   print *, c_int
   if (c_int /= 4) error stop

   print *, c_int64_t
   if (c_int64_t /= 8) error stop

   print *, c_long_long
   if (c_long_long /= 8) error stop

   print *, c_intmax_t
   if (c_intmax_t /= 8) error stop

end program test_iso_c_binding_constants_wasm32
