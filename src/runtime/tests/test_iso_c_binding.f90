program test_iso_c_binding
   use iso_c_binding
   implicit none

   real(c_float) :: r4
   real(c_double) :: r8
   integer(c_long) :: i4
   integer(c_long_long) :: i8

   integer(c_int) :: vi
   integer(c_short) :: vs
   integer(c_long) :: vl
   integer(c_long_long) :: vll
   integer(c_signed_char) :: vsc
   integer(c_size_t) :: vst
   integer(c_int8_t) :: vi8
   integer(c_int16_t) :: vi16
   integer(c_int32_t) :: vi32
   integer(c_int64_t) :: vi64
   integer(c_int_least8_t) :: vil8
   integer(c_int_least16_t) :: vil16
   integer(c_int_least32_t) :: vil32
   integer(c_int_least64_t) :: vil64
   integer(c_int_fast8_t) :: vif8
   integer(c_int_fast16_t) :: vif16
   integer(c_int_fast32_t) :: vif32
   integer(c_int_fast64_t) :: vif64
   integer(c_intmax_t) :: vimax
   integer(c_intptr_t) :: viptr
   integer(c_ptrdiff_t) :: vpdiff

   real(c_float) :: vf
   real(c_double) :: vd
   real(c_long_double) :: vld

   complex(c_float_complex) :: vfc

   ! complex(c_double_complex) :: vdc
   ! complex(c_long_double_complex) :: vldc

   logical(c_bool) :: vb
   character(kind=c_char, len=1) :: vch

   print *, kind(r4)
   if (kind(r4) /= 4) error stop

   print *, kind(r8)
   if (kind(r8) /= 8) error stop

   print *, kind(i4)
   if (kind(i4) /= 8) error stop

   print *, kind(i8)
   if (kind(i8) /= 8) error stop

   ! Test all `bind(c)` constants using variables and kind()
   print *, "kind(vi) =", kind(vi)
   if (kind(vi) /= 4) error stop "c_int failed"

   print *, "kind(vs) =", kind(vs)
   if (kind(vs) /= 2) error stop "c_short failed"

   print *, "kind(vl) =", kind(vl)
   if (kind(vl) /= 8) error stop "c_long failed"

   print *, "kind(vll) =", kind(vll)
   if (kind(vll) /= 8) error stop "c_long_long failed"

   print *, "kind(vsc) =", kind(vsc)
   if (kind(vsc) /= 1) error stop "c_signed_char failed"

   print *, "kind(vst) =", kind(vst)
   if (kind(vst) /= 8) error stop "c_size_t failed"

   print *, "kind(vi8) =", kind(vi8)
   if (kind(vi8) /= 1) error stop "c_int8_t failed"

   print *, "kind(vi16) =", kind(vi16)
   if (kind(vi16) /= 2) error stop "c_int16_t failed"

   print *, "kind(vi32) =", kind(vi32)
   if (kind(vi32) /= 4) error stop "c_int32_t failed"

   print *, "kind(vi64) =", kind(vi64)
   if (kind(vi64) /= 8) error stop "c_int64_t failed"

   print *, "kind(vil8) =", kind(vil8)
   if (kind(vil8) /= 1) error stop "c_int_least8_t failed"

   print *, "kind(vil16) =", kind(vil16)
   if (kind(vil16) /= 2) error stop "c_int_least16_t failed"

   print *, "kind(vil32) =", kind(vil32)
   if (kind(vil32) /= 4) error stop "c_int_least32_t failed"

   print *, "kind(vil64) =", kind(vil64)
   if (kind(vil64) /= 8) error stop "c_int_least64_t failed"

   print *, "kind(vif8) =", kind(vif8)
   if (kind(vif8) /= 1) error stop "c_int_fast8_t failed"

   print *, "kind(vif16) =", kind(vif16)
   if (kind(vif16) /= 2) error stop "c_int_fast16_t failed"

   print *, "kind(vif32) =", kind(vif32)
   if (kind(vif32) /= 4) error stop "c_int_fast32_t failed"

   print *, "kind(vif64) =", kind(vif64)
   if (kind(vif64) /= 8) error stop "c_int_fast64_t failed"

   print *, "kind(vimax) =", kind(vimax)
   if (kind(vimax) /= 8) error stop "c_intmax_t failed"

   print *, "kind(viptr) =", kind(viptr)
   if (kind(viptr) /= 8) error stop "c_intptr_t failed"

   print *, "kind(vpdiff) =", kind(vpdiff)
   if (kind(vpdiff) /= 8) error stop "c_ptrdiff_t failed"

   print *, "kind(vf) =", kind(vf)
   if (kind(vf) /= 4) error stop "c_float failed"

   print *, "kind(vd) =", kind(vd)
   if (kind(vd) /= 8) error stop "c_double failed"

   print *, "kind(vld) =", kind(vld)
   if (kind(vld) /= 8) error stop "c_long_double failed"

   print *, "kind(vfc) =", kind(vfc)
   if (kind(vfc) /= 4) error stop "c_float_complex failed"

   ! print *, "kind(vdc) =", kind(vdc)
   ! if (kind(vdc) /= 8) error stop "c_double_complex failed"
   
   ! print *, "kind(vldc) =", kind(vldc)
   ! if (kind(vldc) /= 8) error stop "c_long_double_complex failed"

   print *, "kind(vb) =", kind(vb)
   if (kind(vb) /= 1) error stop "c_bool failed"

   print *, "kind(vch) =", kind(vch)
   if (kind(vch) /= 1) error stop "c_char failed"

   ! Test null character constant
   print *, "c_null_char =", c_null_char
   if (c_null_char /= char(0)) error stop "c_null_char failed"

end program test_iso_c_binding
