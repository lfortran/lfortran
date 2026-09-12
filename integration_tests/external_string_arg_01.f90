program external_string_arg_01
   use iso_c_binding, only: c_char, c_int32_t
   implicit none
   interface
      subroutine ext_check_char(c, expected_code, status) bind(c)
         import :: c_char, c_int32_t
         character(kind=c_char), intent(in) :: c
         integer(c_int32_t), intent(in) :: expected_code
         integer(c_int32_t), intent(out) :: status
      end subroutine ext_check_char
   end interface
   integer(c_int32_t) :: status
   call ext_check_char(c_char_'N', iachar('N', kind=c_int32_t), status)
   if (status /= 0) error stop "letter 'N' not received correctly"
   call ext_check_char(c_char_'T', iachar('T', kind=c_int32_t), status)
   if (status /= 0) error stop "letter 'T' not received correctly"
end program external_string_arg_01
