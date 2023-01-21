module fpm_filesystem
use fpm_strings, only: f_string, string_t
use iso_c_binding, only: c_ptr, c_associated
implicit none
public :: list_files

contains

recursive subroutine list_files()
    type(c_ptr) :: dir_handle
    type(c_ptr) :: dir_entry_c
    character(len=:), allocatable :: string_fortran
    do
        if (.not. c_associated(dir_entry_c)) then
            exit
        else
            string_fortran = f_string(dir_entry_c)
        end if
    end do
end subroutine list_files

function get_temp_filename() result(tempfile)
    character(:), allocatable :: tempfile
    character(len=1), pointer :: c_tempfile(:)
    tempfile = f_string(c_tempfile)
end function get_temp_filename

end module fpm_filesystem
