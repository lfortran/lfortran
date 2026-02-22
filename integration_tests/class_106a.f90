module class_106a_mod
    implicit none
    private

    type, public, abstract :: AbsType
    contains
        procedure(dummy_ifc), deferred :: dummy
    end type AbsType

    abstract interface
        subroutine dummy_ifc(self)
            import :: AbsType
            class(AbsType), intent(inout) :: self
        end subroutine dummy_ifc
    end interface

    type, public, extends(AbsType) :: ConcreteType
    contains
        procedure :: dummy => concrete_dummy
    end type ConcreteType

    type, public :: MyType
        integer, allocatable        :: ints(:)
        class(AbsType), allocatable :: obj
    contains
        procedure :: my_method
    end type MyType

    interface MyType
        procedure :: constructor
    end interface MyType

contains

    function constructor(ints, obj) result(self)
        integer,        intent(in) :: ints(:)
        class(AbsType), intent(in) :: obj
        type(MyType)               :: self

        self%ints = ints
        self%obj  = obj
    end function constructor

    subroutine my_method(self)
        class(MyType), intent(inout) :: self

        if (.not. allocated(self%ints)) then
            error stop "MyType%ints not allocated"
        end if

        if (.not. allocated(self%obj)) then
            error stop "MyType%obj not allocated"
        end if

        call self%obj%dummy()
    end subroutine my_method

    subroutine concrete_dummy(self)
        class(ConcreteType), intent(inout) :: self
        ! no-op
    end subroutine concrete_dummy

end module class_106a_mod
