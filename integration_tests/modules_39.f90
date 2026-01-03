module modules_39_fpm_39
use modules_39_module_fpm_filesystem_39, only: join_path, filewrite

public :: build_model

contains

subroutine build_model()

    call filewrite(join_path("build", ".gitignore"), ["*"])

end subroutine build_model

end module modules_39_fpm_39

program modules_39
use modules_39_fpm_39
implicit none

call build_model()

print *, "executing modules_39"

end program modules_39
