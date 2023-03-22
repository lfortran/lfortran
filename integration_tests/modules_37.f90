module fpm_backend
implicit none

public :: build_package

type, abstract :: fpm_cmd_settings
    logical :: verbose=.true.
end type

type, extends(fpm_cmd_settings)  :: fpm_build_settings
end type

type string_t
    character(len=:), allocatable :: s
end type

type build_target_ptr
    type(build_target_t), pointer :: ptr => null()
end type build_target_ptr

type build_target_t
    character(:), allocatable :: output_dir
end type build_target_t

type :: fpm_model_t
end type fpm_model_t

contains

subroutine build_package(targets, model, verbose)
    type(build_target_ptr), intent(inout) :: targets(:)
    type(fpm_model_t), intent(in) :: model
    logical, intent(in) :: verbose

    integer :: i, j
    type(string_t), allocatable :: build_dirs(:)
    type(string_t) :: temp

    allocate(build_dirs(0))
    do i = 1, size(targets)
       associate(target => targets(i)%ptr)
          temp%s = target%output_dir
          build_dirs = [build_dirs, temp]
       end associate
    end do

end subroutine build_package

subroutine cmd_build(settings)
    type(fpm_build_settings), intent(in) :: settings
    type(fpm_model_t) :: model
    type(build_target_ptr), allocatable :: targets(:)

    call build_package(targets, model, verbose=settings%verbose)

end subroutine cmd_build

end module fpm_backend
