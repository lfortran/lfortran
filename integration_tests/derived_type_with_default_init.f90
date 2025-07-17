module module_derived_type_with_default_init
    public :: myType, global_myType

    type :: myType
        private 
        logical :: a = .true.
        integer :: i = 2
        character(len=10) :: name = "default"
        integer :: array(2) = [1, 2]
        character(len=2) :: c_array(3) = ['ab', 'bc', 'cd']
        contains
            private
            procedure, public, pass(self) :: config
    end type myType

    type(myType) :: global_myType

contains
    subroutine config(self)
        class(myType), intent(in) :: self
        if (self % a .neqv. .true.) error stop
        if (self % i /= 2) error stop
        if (self % name /= "default") error stop
        if (any(self % array /= [1, 2])) error stop
        ! if (any(self % c_array /= ['ab', 'bc', 'cd'])) error stop ! FIXME: comparion of character arrays not implemented yet
    end subroutine config

end module module_derived_type_with_default_init

program derived_type_with_default_init
    use module_derived_type_with_default_init, global => global_myType
    call global % config()
end program derived_type_with_default_init