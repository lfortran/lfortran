module fpm_targets
implicit none

type int_t
    integer :: i
end type int_t

contains

subroutine prune_build_targets()
    type(int_t) :: modules_used(1)

    contains

    recursive subroutine collect_used_modules()

        modules_used = [modules_used]

    end subroutine collect_used_modules

end subroutine prune_build_targets

end module fpm_targets
