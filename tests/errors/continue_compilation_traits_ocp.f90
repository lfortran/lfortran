! Located diagnostics for the open runtime of protocol traits: a runtime
! generic message erases a binder constrained by a protocol, and each call
! passes the dictionary of its type argument's conformance. Each numbered
! case is an independent violation; all other code is valid.
module cc_traits_ocp_contracts
    implicit none
    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: IScale
        elemental function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface IScale

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: ISquares
        function squares{IScale :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function squares
    end interface ISquares
end module cc_traits_ocp_contracts

module cc_traits_ocp_bag
    use cc_traits_ocp_contracts, only: INumeric
    implicit none
    type, sealed :: Bag
        integer, allocatable :: v(:)
    end type Bag
    implements INumeric :: Bag
        initial :: bag_of
        procedure, static :: operator(+) => joined
    end implements Bag
contains
    function bag_of(n) result(res)
        integer, intent(in) :: n
        type(Bag)           :: res
        res%v = [n]
    end function bag_of
    function joined(lhs, rhs) result(res)
        type(Bag), intent(in) :: lhs, rhs
        type(Bag)             :: res
        res%v = [lhs%v, rhs%v]
    end function joined
end module cc_traits_ocp_bag

module cc_traits_ocp_provider
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Simple
    contains
        procedure, nopass :: sum => simple_sum
    end type Simple
contains
    function simple_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum
end module cc_traits_ocp_provider

! E01: the erased binder's local array has no storage its dictionary describes.
module cc_traits_ocp_storage
    use cc_traits_ocp_contracts, only: INumeric, ISum
    implicit none
    type, sealed, implements(ISum) :: Buffered
    contains
        procedure, nopass :: sum => buffered_sum
    end type Buffered
contains
    function buffered_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        type(T)             :: copy(size(x))
        copy = x
        s = copy(1)
    end function buffered_sum
end module cc_traits_ocp_storage

! E02: an elemental requirement applied to erased arrays computes an array.
module cc_traits_ocp_elemental
    use cc_traits_ocp_contracts, only: IScale, ISquares
    implicit none
    type, sealed, implements(ISquares) :: Squarer
    contains
        procedure, nopass :: squares => first_square
    end type Squarer
contains
    function first{IScale :: U}(y) result(s)
        type(U), intent(in) :: y(:)
        type(U)             :: s
        s = y(1)
    end function first
    function first_square{IScale :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = first(x * x)
    end function first_square
end module cc_traits_ocp_elemental

module cc_traits_ocp_integer
    use cc_traits_ocp_contracts, only: INumeric
    implicit none
    implements INumeric :: integer
    end implements integer
end module cc_traits_ocp_integer

program continue_compilation_traits_ocp
    use cc_traits_ocp_contracts, only: INumeric, ISum
    use cc_traits_ocp_bag, only: Bag
    use cc_traits_ocp_provider, only: Simple
    implicit none
    implements INumeric :: real
    end implements real
    class(ISum), allocatable :: p
    type(Bag) :: b(2), rb
    real :: r(3)
    integer :: k(3)
    p = Simple()
    ! E03: Bag's values own an allocatable component, so its conformance
    ! has no dictionary.
    rb = p%sum(b)
    ! E04: the program's conformance of real has no dictionary.
    print *, p%sum(r)
    ! E05: integer's conformance is declared in a module the program does
    ! not use.
    print *, p%sum(k)
end program continue_compilation_traits_ocp
