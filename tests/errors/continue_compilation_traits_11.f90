! A call through a polymorphic object dispatches through the open slot of a
! generic binding whose type parameter is constrained by an open protocol
! trait (TD-44); a body that cannot be erased is diagnosed once all bodies
! of the translation unit exist.
module continue_compilation_traits_11_traits
    implicit none
    private
    public :: INumeric

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself) :: res
        end function operator(+)
    end interface INumeric
end module continue_compilation_traits_11_traits

module continue_compilation_traits_11_intrinsics
    use continue_compilation_traits_11_traits, only: INumeric

    implements INumeric :: integer
    end implements integer
end module continue_compilation_traits_11_intrinsics

module continue_compilation_traits_11_late
    use continue_compilation_traits_11_traits, only: INumeric
    use continue_compilation_traits_11_intrinsics
    implicit none
    private
    public :: Printer, use_printer

    ! Its body needs a typed copy, so the dispatching call below cannot
    ! reach an erased entry.
    type :: Printer
    contains
        procedure :: first
    end type Printer

contains

    function first{INumeric :: T}(self, x) result(s)
        class(Printer), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(1)
        if (size([x(1), x(1)]) > 2) s = x(2)
    end function first

    integer function use_printer(p) result(r)
        class(Printer), intent(in) :: p
        r = p%first([1, 2])
    end function use_printer
end module continue_compilation_traits_11_late
