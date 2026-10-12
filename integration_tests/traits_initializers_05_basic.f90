! Binds private initializers and a method to MyType without acknowledging
! any trait (traits.tex inline-05).
module traits_initializers_05_basic
    implicit none
    private
    public :: MyType

    type :: MyType
        private
        real :: a = 0.0
    end type MyType

    implements :: MyType
        initial :: init_by_int, init_by_real
        procedure, pass :: value
    end implements MyType

contains

    function init_by_int(n) result(res)
        integer, intent(in) :: n
        type(MyType)        :: res
        res%a = real(n)
    end function init_by_int

    function init_by_real(a) result(res)
        real, intent(in) :: a
        type(MyType)     :: res
        res%a = 10.0 * a
    end function init_by_real

    real function value(self)
        class(MyType), intent(in) :: self
        value = self%a
    end function value
end module traits_initializers_05_basic

! Iface has only a standard constructor interface of its name: its private
! specifics that return exactly type(Iface) are its initializers, the one
! returning an integer is not.
module traits_initializers_05_iface
    implicit none
    private
    public :: Iface

    type :: Iface
        private
        real :: a = 0.0
    contains
        procedure, pass :: value => iface_value
    end type Iface

    interface Iface
        module procedure iface_from_int, iface_from_real, iface_label
    end interface Iface

contains

    function iface_from_int(n) result(res)
        integer, intent(in) :: n
        type(Iface)         :: res
        res%a = real(n) + 0.5
    end function iface_from_int

    function iface_from_real(a) result(res)
        real, intent(in) :: a
        type(Iface)      :: res
        res%a = 2.0 * a
    end function iface_from_real

    integer function iface_label(text)
        character(*), intent(in) :: text
        iface_label = len(text)
    end function iface_label

    real function iface_value(self)
        class(Iface), intent(in) :: self
        iface_value = self%a
    end function iface_value
end module traits_initializers_05_iface
