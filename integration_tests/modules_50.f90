module modules_50_fpm_integer
type int_t
    integer :: i
end type int_t
end module

module modules_50_fpm_targets
use modules_50_fpm_integer, only: int_t
implicit none

contains

subroutine prune_build_targets()
    type(int_t), allocatable :: modules_used(:)

    allocate(modules_used(1))

    modules_used(1)%i = 0
    print *, modules_used
    print *, modules_used(1)%i
    if( modules_used(1)%i /= 0 ) error stop
    call collect_used_modules()
    print *, modules_used
    print *, modules_used(1)%i
    if( modules_used(1)%i /= 2 ) error stop

    contains

    recursive subroutine collect_used_modules()

        modules_used(1)%i = 2
        modules_used = [modules_used]
        ! modules_used = [modules_used, modules_used(1)]

    end subroutine collect_used_modules

end subroutine prune_build_targets

end module modules_50_fpm_targets

program modules_50
use modules_50_fpm_targets
implicit none

call prune_build_targets()

end program modules_50
