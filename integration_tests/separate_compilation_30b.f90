module separate_compilation_30b_module
    use separate_compilation_30a_module, only: abstype, mytype
    implicit none
    private

    type, public :: client
    contains
        procedure :: method
    end type client

    interface client
        procedure :: constructor
    end interface client

contains

    function constructor(b) result(self)
        integer, intent(in) :: b(:)
        type(client) :: self
    end function constructor

    subroutine method(self, arr)
        class(client), intent(in) :: self
        class(mytype), allocatable, intent(out) :: arr(:)

        integer, allocatable :: ints(:)
        class(abstype), allocatable :: obj

        allocate(mytype :: arr(1))
        arr = mytype(ints, obj)
    end subroutine method

end module separate_compilation_30b_module
