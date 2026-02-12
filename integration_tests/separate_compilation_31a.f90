module separate_compilation_31a_module
    implicit none

    type, abstract :: AbsType
    end type AbsType

    type :: MyType
    end type MyType

    interface MyType
        procedure :: constructor
    end interface MyType

contains

    function constructor(obj) result(self)
        class(AbsType), intent(in) :: obj
        type(MyType) :: self
    end function constructor

end module separate_compilation_31a_module
