module separate_compilation_30a_module
    implicit none

    type, abstract :: abstype
    end type abstype

    type :: mytype
        integer, allocatable :: ints(:)
        class(abstype), allocatable :: obj
    end type mytype

    interface mytype
        procedure :: constructor
    end interface mytype

contains

    function constructor(ints, obj) result(self)
        integer, intent(in) :: ints(:)
        class(abstype), intent(in) :: obj
        type(mytype) :: self
    end function constructor

end module separate_compilation_30a_module
