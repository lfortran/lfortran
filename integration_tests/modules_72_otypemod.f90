module modules_72_otypemod
    use modules_72_stypemod
    implicit none

    type :: OtherType
        integer :: y = 0
    end type OtherType

    interface OtherType
        procedure :: constructor
    end interface OtherType

contains

    function constructor() result(self)
        type(OtherType) :: self
        self%y = 20
    end function constructor

end module modules_72_otypemod
