program char_array_initialization_declaration_02
   implicit none

   call test_block_scope()
   call test_program_scope()

contains

   subroutine test_block_scope()
      integer :: i
      methods_block: block
         character(20), dimension(4) :: methods = [ &
              "zero      ", "same      ", "valid     ", "full      " ]
         do i = 1, size(methods)
            if (len(methods(i)) /= 20) error stop "block len"
         end do
         if (len_trim(methods(1)) /= 4) error stop "block trim 1"
         if (len_trim(methods(2)) /= 4) error stop "block trim 2"
         if (len_trim(methods(3)) /= 5) error stop "block trim 3"
         if (len_trim(methods(4)) /= 4) error stop "block trim 4"
         if (trim(methods(1)) /= "zero") error stop "block val 1"
         if (trim(methods(2)) /= "same") error stop "block val 2"
         if (trim(methods(3)) /= "valid") error stop "block val 3"
         if (trim(methods(4)) /= "full") error stop "block val 4"
      end block methods_block
   end subroutine test_block_scope

   subroutine test_program_scope()
      character(20), dimension(4) :: methods = [ &
           "zero      ", "same      ", "valid     ", "full      " ]
      if (len_trim(methods(1)) /= 4) error stop "sub trim 1"
      if (len_trim(methods(2)) /= 4) error stop "sub trim 2"
      if (len_trim(methods(3)) /= 5) error stop "sub trim 3"
      if (len_trim(methods(4)) /= 4) error stop "sub trim 4"
      if (trim(methods(2)) /= "same") error stop "sub val 2"
   end subroutine test_program_scope

end program char_array_initialization_declaration_02
