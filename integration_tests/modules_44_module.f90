module modules_44_module_fpm_filesystem
implicit none

contains

    subroutine warnwrite(fname, data)
        character(len=*), intent(in) :: fname
        character(len=*), intent(in) :: data(:)
    end subroutine warnwrite

end module modules_44_module_fpm_filesystem
