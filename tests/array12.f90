module array12
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_associated
    implicit none

    interface
        function getcwd(buf, bufsize) result(path) bind(C, name="getcwd")
            import :: c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in) :: buf(*)
            integer(c_int), value, intent(in) :: bufsize
            type(c_ptr) :: path
        end function getcwd
   end interface

contains

subroutine get_current_directory(path)
    character(len=:), allocatable, intent(out) :: path
    character(kind=c_char, len=1), allocatable :: cpath(:)
    integer(c_int), parameter :: buffersize = 1000_c_int
    type(c_ptr) :: tmp

    allocate(cpath(buffersize))

    tmp = getcwd(cpath, buffersize)
    if (c_associated(tmp)) then
       print *, "PWD: ", tmp
    end if
end subroutine get_current_directory

end module array12
