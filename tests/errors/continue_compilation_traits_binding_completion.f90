! Located diagnostics for binding procedures and initializers to a type with
! an implements statement, and for completing the conformances that rely on
! them. Each numbered case is an independent violation; all other code is
! valid.
module cc_traits_binding_completion_contracts
    implicit none
    abstract interface :: IConstructable
        initial(n)
            integer, intent(in) :: n
        end initial
    end interface IConstructable

    abstract interface :: IPrintable
        subroutine output()
        end subroutine output
    end interface IPrintable

contains

    function make{IConstructable :: T}(n) result(r)
        integer, intent(in) :: n
        type(T)             :: r
        r = T(n)
    end function make
end module cc_traits_binding_completion_contracts

module cc_traits_binding_completion_bound
    implicit none
    type :: Bound
        private
        real :: a = 0.0
    end type Bound

    implements :: Bound
        initial :: bound_from_integer
        procedure, pass :: output => bound_output
    end implements Bound
contains
    function bound_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Bound)         :: res
        res%a = real(n)
    end function bound_from_integer

    subroutine bound_output(self)
        class(Bound), intent(in) :: self
        print *, self%a
    end subroutine bound_output
end module cc_traits_binding_completion_bound

! E01: binding procedures without traits acknowledges no trait, even when
! the signatures match: Bound neither satisfies the constraint of make nor
! forms an IPrintable view.
program cc_traits_binding_completion_nominal
    use cc_traits_binding_completion_contracts, only: IPrintable, make
    use cc_traits_binding_completion_bound, only: Bound
    implicit none
    type(Bound) :: b
    class(IPrintable), allocatable :: printer
    b = make{Bound}(3)
    allocate(printer, source=Bound(4))
end program cc_traits_binding_completion_nominal

! E02: procedures are bound without traits only to a type of the unit.
module cc_traits_binding_completion_foreign
    use cc_traits_binding_completion_bound, only: Bound
    implicit none
    implements :: Bound
        procedure, pass :: twice
    end implements Bound
contains
    subroutine twice(self)
        class(Bound), intent(in) :: self
        call self%output()
        call self%output()
    end subroutine twice
end module cc_traits_binding_completion_foreign

! E03: the type already binds the name output.
module cc_traits_binding_completion_duplicate
    implicit none
    type :: Twice
        integer :: n = 0
    contains
        procedure, pass :: output => first_output
    end type Twice

    implements :: Twice
        procedure, pass :: output => second_output
    end implements Twice
contains
    subroutine first_output(self)
        class(Twice), intent(in) :: self
        print *, self%n
    end subroutine first_output

    subroutine second_output(self)
        class(Twice), intent(in) :: self
        print *, -self%n
    end subroutine second_output
end module cc_traits_binding_completion_duplicate

! E04: without a trait there is no requirement for a static binding to
! implement.
module cc_traits_binding_completion_static
    implicit none
    type, sealed :: Made
        integer :: n = 0
    end type Made

    implements :: Made
        procedure, static :: create => create_made
    end implements Made
contains
    function create_made() result(res)
        type(Made) :: res
        res%n = 1
    end function create_made
end module cc_traits_binding_completion_static

! E05: an implements statement does not declare deferred bindings.
module cc_traits_binding_completion_deferred
    implicit none
    type :: Open
        integer :: n = 0
    end type Open

    abstract interface
        subroutine open_output(self)
            import :: Open
            class(Open), intent(in) :: self
        end subroutine open_output
    end interface

    implements :: Open
        procedure(open_output), deferred, pass :: output
    end implements Open
end module cc_traits_binding_completion_deferred
