program test_iso_c_binding_constants_01
  use, intrinsic :: iso_c_binding, only: c_intmax_t, &
          c_alert, c_backspace, c_form_feed, c_carriage_return, &
          c_horizontal_tab, c_vertical_tab

! References:
! ifx  https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2025-3/named-constants-in-the-iso-c-binding-module.html
! gfortran  https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
! flang https://github.com/llvm/llvm-project/blob/main/flang/module/iso_c_binding.f90

   print *, "c_intmax_t = ", c_intmax_t
   if (c_intmax_t /= 8) error stop

   print *, "c_alert = ", c_alert
   if (c_alert /= char(7)) error stop
   
   print *, "c_backspace = ", c_backspace
   if (c_backspace /= char(8)) error stop
   
   print *, "c_form_feed = ", c_form_feed
   if (c_form_feed /= char(12)) error stop
   
   print *, "c_carriage_return = ", c_carriage_return
   if (c_carriage_return /= char(13)) error stop
   
   print *, "c_horizontal_tab = ", c_horizontal_tab
   if (c_horizontal_tab /= char(9)) error stop
   
   print *, "c_vertical_tab = ", c_vertical_tab
   if (c_vertical_tab /= char(11)) error stop
    
end program test_iso_c_binding_constants_01

