module separate_compilation_43a_module
    implicit none

    type, abstract :: AbsType
        procedure(pintfc), pointer :: ptr => null()
    end type AbsType

    abstract interface
        subroutine pintfc(self)
            import
            class(AbsType), intent(in) :: self
        end subroutine pintfc
    end interface

end module separate_compilation_43a_module
