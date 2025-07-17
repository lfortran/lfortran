module module_derived_type_with_default_init_01
    public :: myType, global_myType

    type :: myType 
        logical :: a = .true.
        integer :: i = 2
        character(len=10) :: name = "default"
        integer :: array(2) = [1, 2]
        character(len=1) :: c_array(3) = ['a', 'b', 'c']
    end type myType

    type(myType) :: global_myType
end module module_derived_type_with_default_init_01

program derived_type_with_default_init_01
    use module_derived_type_with_default_init_01, global => global_myType
    if (global % a .neqv. .true.) error stop
    if (global % i /= 2) error stop
    if (global % name /= "default") error stop
    if (any(global % array /= [1, 2])) error stop
    print *, global % c_array
    ! if (any(global % c_array /= ['a', 'b', 'c'])) error stop ! FIXME: comparion of character arrays not implemented yet
end program derived_type_with_default_init_01