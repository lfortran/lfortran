module derived_types_74_mod
    implicit none

    type :: string_t
        character(len=:), allocatable :: value
    end type

    type, abstract :: base
    end type

    type :: source_t
        type(string_t), allocatable :: modules_provided(:)
    end type

    type, extends(base) :: package_t
        type(source_t), allocatable :: sources(:)
    end type

    type :: model_t
        type(package_t), allocatable :: packages(:)
    end type
end module


program derived_types_74
    use derived_types_74_mod
    implicit none

    type(string_t) :: module_name
    type(package_t), allocatable :: packages(:)
    integer :: i

    allocate(packages(1))
    allocate(packages(1)%sources(1))
    allocate(packages(1)%sources(1)%modules_provided(1))
    packages(1)%sources(1)%modules_provided(1)%value = "module_abc"
    module_name = packages(1)%sources(1)%modules_provided(1)
    if (module_name%value /= "module_abc") error stop
end program
