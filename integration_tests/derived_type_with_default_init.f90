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
        print *, "a = ", self%a
        print *, "i = ", self%i        
    end subroutine config

end module module_derived_type_with_default_init

program main
    use module_derived_type_with_default_init, global => global_myType
    call global % config()
end program main