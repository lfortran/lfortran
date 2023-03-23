module fpm_filesystem_39

contains

function join_path(a1, a2, a3, a4, a5) result(path)
    character(len=*), intent(in) :: a1, a2
    character(len=*), intent(in), optional :: a3, a4, a5
    character(len=:), allocatable :: path
end function join_path

subroutine filewrite(filename, filedata)
character(len=*), intent(in) :: filename
character(len=*), intent(in) :: filedata(:)

end subroutine filewrite

end module fpm_filesystem_39
