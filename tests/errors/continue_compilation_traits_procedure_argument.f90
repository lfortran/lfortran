! Conformance diagnostics of trait messages with dummy procedures name each
! dummy procedure by its interface.
module continue_compilation_traits_procedure_argument_m
    implicit none
    abstract interface
        integer function iunary(n)
            integer, intent(in) :: n
        end function
        real function ireal(n)
            integer, intent(in) :: n
        end function
    end interface
    abstract interface :: IApply
        integer function apply(self, f)
            import
            type(itself), pass, intent(in) :: self
            procedure(iunary), pointer, intent(in) :: f
        end function
    end interface
    abstract interface :: ICall
        integer function call_with(self, f)
            import
            type(itself), pass, intent(in) :: self
            procedure(iunary) :: f
        end function
    end interface
    type, implements(IApply) :: WrongInterface
    contains
        procedure :: apply => wrong_interface
    end type
    type, implements(IApply) :: NotPointer
    contains
        procedure :: apply => not_pointer
    end type
    type, implements(ICall) :: PointerInstead
    contains
        procedure :: call_with => pointer_instead
    end type
contains
    integer function wrong_interface(self, f)
        class(WrongInterface), intent(in) :: self
        procedure(ireal), pointer, intent(in) :: f
        wrong_interface = int(f(1))
    end function
    integer function not_pointer(self, f)
        class(NotPointer), intent(in) :: self
        procedure(iunary) :: f
        not_pointer = f(1)
    end function
    integer function pointer_instead(self, f)
        class(PointerInstead), intent(in) :: self
        procedure(iunary), pointer, intent(in) :: f
        pointer_instead = f(1)
    end function
end module
