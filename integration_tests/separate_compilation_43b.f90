module separate_compilation_43b_module
    use separate_compilation_43a_module
    implicit none

    type :: MyType
        class(AbsType), allocatable :: obj
    contains
        procedure :: method
    end type MyType

contains

    subroutine method(self)
        class(MyType), intent(in) :: self
        call self%obj%ptr()
    end subroutine method

end module separate_compilation_43b_module
