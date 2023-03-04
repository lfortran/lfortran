module arrays_23
implicit none
   type :: toml_context

      !> Current internal position
      integer :: pos

      !> Current internal count
      integer :: num

      !> Current internal location on the string buffer
      character(len=:), pointer :: ptr

   end type toml_context
contains

subroutine add_context(message, context)

   !> A detailed message describing the error, requiring some more context
   character(len=:), allocatable, intent(inout) :: message

   !> Current context producing the error
   type(toml_context), intent(in) :: context

   character(len=20) :: num
   integer :: line_break

   if (context%num > 0) then
      write(num, '("line",1x,i0,":")') context%num
      message = num(1:len_trim(num)+1) // message
   end if

   message = message // &
      & '   | '// context%ptr(1:line_break) // &
      & '   |'

end subroutine add_context
end module arrays_23


program main
   use arrays_23
   implicit none
   print *, "working"
end program main
