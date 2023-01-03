module arrays_22
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_associated
    implicit none
    type :: toml_context

        !> Current internal position
        integer :: pos

        !> Current internal count
        integer :: num

        !> Current internal location on the string buffer
        character(len=:), pointer :: ptr

    end type toml_context

    interface
        function getcwd(buf, bufsize) result(path) bind(C, name="getcwd")
            import :: c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in) :: buf(*)
            integer(c_int), value, intent(in) :: bufsize
            type(c_ptr) :: path
        end function getcwd
   end interface

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

subroutine get_current_directory(path)
    character(len=:), allocatable, intent(out) :: path
    character(kind=c_char, len=1), allocatable :: cpath(:)
    integer(c_int), parameter :: buffersize = 1000_c_int
    type(c_ptr) :: tmp

    allocate(cpath(buffersize))

    tmp = getcwd(cpath, buffersize)
    if (c_associated(tmp)) then
       call c_f_character(cpath, path)
    end if
end subroutine get_current_directory

end module arrays_22
