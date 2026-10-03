module mytypemod

    type, abstract :: AbsType
    end type AbsType

    type :: MyType
    end type MyType

    interface MyType
        procedure :: constructor
    end interface MyType

contains

    function constructor(obj) result(self)
        type(MyType) :: self
        class(AbsType), intent(in) :: obj
    end function constructor

end module mytypemod

module clientmod

    use mytypemod, only: AbsType, MyType

contains

    function get_my_obj() result(myobj)
        class(MyType), allocatable :: myobj
        myobj = MyType(get_obj())
    end function get_my_obj

    function get_obj() result(obj)
        class(AbsType), allocatable :: obj
    end function get_obj

end module clientmod

program test
    use clientmod
end program test
