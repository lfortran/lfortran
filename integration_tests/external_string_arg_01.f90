program external_string_arg_01
   implicit none
   interface
      subroutine ext_check_char(c, expected_code, status)
         character(len=1), intent(in) :: c
         integer, intent(in) :: expected_code
         integer, intent(out) :: status
      end subroutine ext_check_char
   end interface
   integer :: status
   call ext_check_char('N', iachar('N'), status)
   if (status /= 0) error stop "letter 'N' not received correctly"
   call ext_check_char('T', iachar('T'), status)
   if (status /= 0) error stop "letter 'T' not received correctly"
end program external_string_arg_01
