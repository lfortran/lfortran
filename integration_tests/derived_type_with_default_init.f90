module module_derived_type_with_default_init
    public :: myType, global_myType

    type :: myType
        private 
        logical :: a = .true.
        integer :: i = 2

        contains
            private
            procedure, public, pass(self) :: config
    end type myType

    type(myType) :: global_myType

contains
    subroutine config(self)
        class(myType), intent(in) :: self
        if (self % a .neqv. .true.) error stop
        if (self % i .ne. 2) error stop
    end subroutine config

end module module_derived_type_with_default_init

program derived_type_with_default_init
    use module_derived_type_with_default_init, global => global_myType
    call global % config()
end program derived_type_with_default_init