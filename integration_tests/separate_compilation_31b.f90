module separate_compilation_31b_module
    use separate_compilation_31a_module, only: AbsType, MyType
    implicit none

contains

    function get_my_obj() result(myobj)
        class(MyType), allocatable :: myobj
        myobj = MyType(get_obj())
    end function get_my_obj

    function get_obj() result(obj)
        class(AbsType), allocatable :: obj
    end function get_obj

end module separate_compilation_31b_module
