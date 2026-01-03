module fpm_model_modules_33
use fpm_dependency_modules_33, only: dependency_tree_t
implicit none

type :: fpm_model_t
    character(:), allocatable :: package_name
    character(:), allocatable :: fortran_compile_flags
    character(:), allocatable :: c_compile_flags
    character(:), allocatable :: cxx_compile_flags
    character(:), allocatable :: link_flags
    character(:), allocatable :: build_prefix
    type(dependency_tree_t) :: deps
    logical :: include_tests = .true.
end type fpm_model_t

end module fpm_model_modules_33
