module fpm_targets
implicit none

type build_target_ptr
    type(build_target_t), pointer :: ptr => null()
end type build_target_ptr

type build_target_t
    character(:), allocatable :: output_file
    character(:), allocatable :: version
end type build_target_t


end module fpm_targets
