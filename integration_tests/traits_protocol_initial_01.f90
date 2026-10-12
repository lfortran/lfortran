! The initializer of an implements statement overloads the constructor of a
! type of its unit whether the statement precedes or follows the type:
! concrete Fixed(3) and generic T(3) both call it, while a keyword reference
! that it does not accept remains the structure constructor.
module traits_protocol_initial_01_contracts
    implicit none
    private
    public :: IInit, make

    abstract interface :: IInit
        initial(n)
            integer, intent(in) :: n
        end initial
    end interface IInit

contains

    function make{IInit :: T}(n) result(r)
        integer, intent(in) :: n
        type(T)             :: r
        r = T(n)
    end function make
end module traits_protocol_initial_01_contracts

module traits_protocol_initial_01_types
    use traits_protocol_initial_01_contracts, only: IInit
    implicit none
    private

    implements IInit :: Fixed
        initial :: fixed_from_integer
    end implements Fixed

    type, public, sealed :: Fixed
        integer :: tenths = 0
    end type Fixed

    type, public, sealed :: Later
        integer :: tenths = 0
    end type Later

    implements IInit :: Later
        initial :: later_from_integer
    end implements Later

contains

    function fixed_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Fixed)         :: res
        res%tenths = 10 * n
    end function fixed_from_integer

    function later_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Later)         :: res
        res%tenths = 10 * n
    end function later_from_integer
end module traits_protocol_initial_01_types

program traits_protocol_initial_01
    use traits_protocol_initial_01_contracts, only: make
    use traits_protocol_initial_01_types, only: Fixed, Later
    implicit none
    type(Fixed) :: f
    type(Later) :: g

    f = Fixed(3)
    if (f%tenths /= 30) error stop 1
    f = make{Fixed}(3)
    if (f%tenths /= 30) error stop 2
    f = Fixed(tenths=3)
    if (f%tenths /= 3) error stop 3

    g = Later(3)
    if (g%tenths /= 30) error stop 4
    g = make{Later}(3)
    if (g%tenths /= 30) error stop 5
    g = Later(tenths=3)
    if (g%tenths /= 3) error stop 6
    print '(a)', 'traits_protocol_initial_01 ok'
end program traits_protocol_initial_01
