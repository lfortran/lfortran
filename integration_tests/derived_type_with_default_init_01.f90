module module_derived_type_with_default_init_01
    public :: myType, global_myType

    type :: myType 
        logical :: a = .true.
        integer :: i = 2
    end type myType

    type(myType) :: global_myType
end module module_derived_type_with_default_init_01

program derived_type_with_default_init_01
    use module_derived_type_with_default_init_01, global => global_myType
    if (global % a .neqv. .true.) error stop
    if (global % i .ne. 2) error stop
end program derived_type_with_default_init_01