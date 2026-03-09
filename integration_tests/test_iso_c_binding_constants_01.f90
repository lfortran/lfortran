module iso_c_const_01
  implicit none

! Expected test FAIL iso_c_binding_constants
! References:
! ifx  https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2025-3/named-constants-in-the-iso-c-binding-module.html
! gfortran  https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
! flang https://github.com/llvm/llvm-project/blob/main/flang/module/iso_c_binding.f90

  type :: c_ptr
    integer ptr
  end type

  type :: c_funptr
    integer ptr
  end type


  integer, parameter :: c_int = 4, c_short = 2, c_long = 8, c_long_long = 8
  integer, parameter :: c_signed_char = 1
  integer, parameter :: c_size_t=8 
  integer, parameter :: c_int8_t = 1, c_int16_t = 2, c_int32_t = 4, c_int64_t = 8
  integer, parameter :: c_int_least8_t = -4 , c_int_least16_t = -4, c_int_least32_t = -4, c_int_least64_t = -4
  integer, parameter :: c_int_fast8_t = -4, c_int_fast16_t = -4, c_int_fast32_t = -4, c_int_fast64_t = -4
  integer, parameter :: c_intmax_t = -4
  integer, parameter :: c_intptr_t = 8, c_ptrdiff_t = 8
  integer, parameter :: c_float = 4, c_double = 8, c_float_complex = 4, c_double_complex = 8 
  integer, parameter :: c_long_double = -4, c_long_double_complex = -4
  integer, parameter :: c_bool = 1, c_char = 1

  character(len=1), parameter :: c_null_char = char(0)
  character(len=1), parameter :: c_alert = char(0)
  character(len=1), parameter :: c_backspace = char(0)
  character(len=1), parameter :: c_form_feed = char(0)
  character(len=1), parameter :: c_new_line = char(10)
  character(len=1), parameter :: c_carriage_return = char(0)
  character(len=1), parameter :: c_horizontal_tab = char(0)
  character(len=1), parameter :: c_vertical_tab = char(0)
  
  type(c_ptr), parameter :: c_null_ptr = c_ptr(0)
  type(c_funptr), parameter :: c_null_funptr = c_funptr(0)

  interface
    logical function c_associated(c_ptr_1)
    import c_ptr
    type(c_ptr), intent(in) :: c_ptr_1
    end function

    subroutine c_f_pointer(cptr, fptr, shape)
    import c_ptr
    type(c_ptr), intent(in) :: cptr
    !type(*), pointer, intent(out) :: fptr
    integer, pointer, intent(out) :: fptr
    integer, intent(in), optional :: shape(:)
    end subroutine

    subroutine c_f_procpointer(cptr, fptr)
    import c_funptr
    type(c_funptr), intent(in) :: cptr
    !procedure(*), pointer, intent(out) :: fptr
    integer, pointer, intent(out) :: fptr
    end subroutine

    !type(c_ptr) function c_loc(x)
    integer function c_loc(x)
    import c_ptr
    !type(*), intent(in) :: x
    integer, intent(in) :: x
    end function

    !type(c_funptr) function c_funloc(x)
    integer function c_funloc(x)
    import c_funptr
    !type(*), intent(in) :: x
    integer, intent(in) :: x
    end function
  end interface

end module iso_c_const_01

program test_iso_c_binding_constants_01
  use iso_c_const_01

  print *, "c_int = ", c_int
  if (c_int /= 4)  error stop
   
  print *, "c_short = ", c_short
  if (c_short /= 2) error stop
   
   print *, "c_long = ", c_long
   if (c_long /= 8) error stop
   
   print *, "c_long_long = ", c_long_long
   if (c_long_long /= 8) error stop
   
   print *, "c_signed_char = ", c_signed_char
   if (c_signed_char /= 1) error stop

   print *, "c_size_t = ", c_size_t
   if (c_size_t /= 8) error stop
   
   print *, "c_int8_t = ", c_int8_t
   if (c_int8_t /= 1) error stop
   
   print *, "c_int16_t = ", c_int16_t
   if (c_int16_t /= 2) error stop
   
   print *, "c_int32_t = ", c_int32_t
   if (c_int32_t /= 4) error stop
   
   print *, "c_int64_t = ",  c_int64_t
   if (c_int64_t /= 8) error stop

   print *, "c_int_least8_t = ", c_int_least8_t
   if (c_int_least8_t /= 1) error stop
   
   print *, "c_int_least16_t = ",  c_int_least16_t
   if (c_int_least16_t /= 2) error stop
   
   print *, "c_int_least32_t = ", c_int_least32_t
   if (c_int_least32_t /= 4) error stop
   
   print *, "c_int_least64_t = ", c_int_least64_t
   if (c_int_least64_t /= 8) error stop

   print *, "c_int_fast8_t = ", c_int_fast8_t
   if (c_int_fast8_t /= 1) error stop
   
   print *, "c_int_fast16_t = ", c_int_fast16_t
   if (c_int_fast16_t /= 2) error stop
   
   print *, "c_int_fast32_t = ", c_int_fast32_t
   if (c_int_fast32_t /= 4) error stop
   
   print *, "c_int_fast64_t = ", c_int_fast64_t
   if (c_int_fast64_t /= 8) error stop

   print *, "c_intmax_t = ", c_intmax_t
   if (c_intmax_t /= 8) error stop
   
   print *, "c_intptr_t = ", c_intptr_t
   if (c_intptr_t /= 8) error stop
   
   print *, "c_ptrdiff_t = ", c_ptrdiff_t
   if (c_ptrdiff_t /= 8) error stop

   print *, "c_float = ", c_float
   if (c_float /= 4) error stop
   
   print *, "c_double = ", c_double
   if (c_double /= 8) error stop
   
   print *, "c_long_double = ", c_long_double
   if (c_long_double /= -1) error stop   ! Currently unsupported
   
   print *, "c_float_complex = ", c_float_complex
   if (c_float_complex /= 4) error stop
   
   print *, "c_double_complex = ", c_double_complex
   if (c_double_complex /= 8) error stop
   
   print *, "c_long_double_complex = ", c_long_double_complex
   if (c_long_double_complex /= -1) error stop ! Currently unsupported

   print *, "c_bool = ", c_bool
   if (c_bool /= 1) error stop
   
   print *, "c_char = ", c_char
   if (c_char /= 1) error stop

   !! character types
   print *, "c_null_char = ", c_null_char
   if (c_null_char /= char(0)) error stop
   
   print *, "c_alert = ", c_alert
   if (c_alert /= char(7)) error stop
   
   print *, "c_backspace = ", c_backspace
   if (c_backspace /= char(8)) error stop
   
   print *, "c_form_feed = ", c_form_feed
   if (c_form_feed /= char(12)) error stop
   
   print *, "c_new_line = ", c_new_line
   if (c_new_line /= char(10)) error stop
   
   print *, "c_carriage_return = ", c_carriage_return
   if (c_carriage_return /= char(13)) error stop
   
   print *, "c_horizontal_tab = ", c_horizontal_tab
   if (c_horizontal_tab /= char(9)) error stop
   
   print *, "c_vertical_tab = ", c_vertical_tab
   if (c_vertical_tab /= char(11)) error stop
    
end program test_iso_c_binding_constants_01

