module modules_42_fpm_targets
implicit none

type int_t
    integer :: i
end type int_t

contains

subroutine prune_build_targets()
    type(int_t) :: modules_used(1)

    modules_used(1)%i = 0
    call collect_used_modules()
    print *, modules_used(1)%i

    contains

    recursive subroutine collect_used_modules()

        modules_used = [modules_used(1)]

    end subroutine collect_used_modules

end subroutine prune_build_targets

end module modules_42_fpm_targets

program modules_42
use modules_42_fpm_targets
implicit none

call prune_build_targets()

end program modules_42
