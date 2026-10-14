! Initializer requirements are implemented by the initializers a type already
! binds (traits.tex, "Binding functionality to a type" and "Use with the
! sealed attribute"): those of a sealed type's definition that adopts the
! trait, and those an implements statement without traits bound when a later
! empty implements statement of the same module acknowledges the trait.
! Concrete constructor calls and generic T(args) select the same initializer.
module traits_initializers_04_contracts
    implicit none
    private
    public :: IConstructable, IValued, from_int, from_real

    abstract interface :: IConstructable
        initial(n)
            integer, intent(in) :: n
        end initial
        initial(a)
            real, intent(in) :: a
        end initial
    end interface IConstructable

    abstract interface :: IValued
        function value(self) result(r)
            type(itself), intent(in), pass :: self
            real :: r
        end function value
    end interface IValued

contains

    function from_int{IConstructable :: T}(n) result(r)
        integer, intent(in) :: n
        type(T)             :: r
        r = T(n)
    end function from_int

    function from_real{IConstructable :: T}(a) result(r)
        real, intent(in) :: a
        type(T)          :: r
        r = T(a)
    end function from_real
end module traits_initializers_04_contracts

module traits_initializers_04_types
    use traits_initializers_04_contracts, only: IConstructable, IValued
    implicit none
    private
    public :: Sealed, Later

    type, sealed, implements(IConstructable + IValued) :: Sealed
        private
        real :: a = 0.0
    contains
        initial :: sealed_from_int, sealed_from_real
        procedure, pass :: value => sealed_value
    end type Sealed

    type :: Later
        private
        real :: a = 0.0
    end type Later

    implements :: Later
        initial :: later_from_int, later_from_real
        procedure, pass :: value => later_value
    end implements Later

    implements (IConstructable + IValued) :: Later
    end implements Later

contains

    function sealed_from_int(n) result(res)
        integer, intent(in) :: n
        type(Sealed)        :: res
        res%a = real(n) + 0.25
    end function sealed_from_int

    function sealed_from_real(a) result(res)
        real, intent(in) :: a
        type(Sealed)     :: res
        res%a = a + 0.5
    end function sealed_from_real

    real function sealed_value(self)
        type(Sealed), intent(in) :: self
        sealed_value = self%a
    end function sealed_value

    function later_from_int(n) result(res)
        integer, intent(in) :: n
        type(Later)         :: res
        res%a = 10.0 * real(n)
    end function later_from_int

    function later_from_real(a) result(res)
        real, intent(in) :: a
        type(Later)      :: res
        res%a = 100.0 * a
    end function later_from_real

    real function later_value(self)
        class(Later), intent(in) :: self
        later_value = self%a
    end function later_value
end module traits_initializers_04_types

program traits_initializers_04
    use traits_initializers_04_contracts, only: from_int, from_real
    use traits_initializers_04_types, only: Sealed, Later
    implicit none
    type(Sealed) :: s
    type(Later) :: l

    s = Sealed(3)
    if (abs(s%value() - 3.25) > 1e-6) error stop 1
    s = from_int{Sealed}(3)
    if (abs(s%value() - 3.25) > 1e-6) error stop 2
    s = Sealed(2.5)
    if (abs(s%value() - 3.0) > 1e-6) error stop 3
    s = from_real{Sealed}(2.5)
    if (abs(s%value() - 3.0) > 1e-6) error stop 4

    l = Later(3)
    if (abs(l%value() - 30.0) > 1e-6) error stop 5
    l = from_int{Later}(3)
    if (abs(l%value() - 30.0) > 1e-6) error stop 6
    l = Later(2.5)
    if (abs(l%value() - 250.0) > 1e-6) error stop 7
    l = from_real{Later}(2.5)
    if (abs(l%value() - 250.0) > 1e-6) error stop 8
    print '(a)', 'traits_initializers_04 ok'
end program traits_initializers_04
