module fpm_39
use fpm_filesystem_39, only: join_path, filewrite

public :: build_model

contains

subroutine build_model()

    call filewrite(join_path("build", ".gitignore"), ["*"])

end subroutine build_model

end module fpm_39
