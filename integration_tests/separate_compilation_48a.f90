module separate_compilation_48a_types
implicit none
type :: string_t
end type
type :: srcfile_t
end type
type :: package_t
    character(:), allocatable :: name
    type(srcfile_t), allocatable :: sources(:)
end type
type :: fpm_model_t
    type(package_t), allocatable :: packages(:)
end type
end module
