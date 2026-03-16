module modules_72_stypemod
    implicit none
    private

    type, public :: SomeType
        integer :: x = 0
    end type SomeType

    interface SomeType
        procedure :: constructor
    end interface SomeType

contains

    function constructor() result(self)
        type(SomeType) :: self
        self%x = 10
    end function constructor

end module modules_72_stypemod
