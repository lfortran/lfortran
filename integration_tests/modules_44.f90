module modules_44_fpm_cmd_new
use modules_44_module_fpm_filesystem, only : warnwrite
implicit none

type, abstract :: fpm_cmd_settings
    character(len=:), allocatable :: working_dir
end type

type, extends(fpm_cmd_settings)  :: fpm_new_settings
    character(len=:), allocatable :: name
end type

contains

subroutine cmd_new(settings)
    type(fpm_new_settings), intent(in) :: settings
    character(len=:), allocatable :: littlefile(:)

    littlefile = [character(len=80) :: '# ', 'My cool new project!']

    call warnwrite(settings%name, littlefile)
    ! call warnwrite(settings%name, littlefile)

end subroutine cmd_new

end module modules_44_fpm_cmd_new

program modules_44
use modules_44_fpm_cmd_new
implicit none

type(fpm_new_settings) :: settings
call cmd_new(settings)

end program modules_44
