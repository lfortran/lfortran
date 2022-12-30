module arrays_22
implicit none
   type :: toml_context

      !> Current internal position
      integer :: pos = 0

      !> Current internal count
      integer :: num = 0

      !> Current internal location on the string buffer
      character(kind=tfc, len=:), pointer :: ptr => null()

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

   if (associated(context%ptr)) then
      ! if (line_break < 0) line_break = len(context%ptr)
      message = message // &
         & '   | '// context%ptr(1:line_break) // &
         & '   |'
      ! if (context%pos > 0 .and. context%pos <= line_break) then
      !    message = message // repeat('-', context%pos) // '^'
      ! end if
   end if

end subroutine add_context
end module arrays_22
