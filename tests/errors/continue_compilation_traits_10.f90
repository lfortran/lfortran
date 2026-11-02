! Overriding generic bindings whose type parameter is constrained by an open
! protocol trait (TD-44): the open slot needs the procedure erased with the
! dictionaries of its type parameters.
module continue_compilation_traits_10_traits
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
end module continue_compilation_traits_10_traits

module continue_compilation_traits_10_base
    use continue_compilation_traits_10_traits, only: INumeric
    implicit none
    private
    public :: Base

    type :: Base
    contains
        procedure :: count
        procedure :: total
    end type Base

contains

    ! A local array of the type parameter cannot be erased: no open slot.
    function count{INumeric :: T}(self, x) result(n)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        integer :: n
        type(T) :: copy(size(x))
        copy = x
        n = size(copy)
    end function count

    function total{INumeric :: T}(self, x) result(s)
        class(Base), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(1)
    end function total
end module continue_compilation_traits_10_base

module continue_compilation_traits_10_child
    use continue_compilation_traits_10_traits, only: INumeric
    use continue_compilation_traits_10_base, only: Base
    implicit none
    private
    public :: Copied

    ! Overrides with a procedure that cannot be erased.
    type, extends(Base) :: Copied
    contains
        procedure :: total => copied_total
    end type Copied

contains

    function copied_total{INumeric :: T}(self, x) result(s)
        class(Copied), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        type(T) :: copy(size(x))
        copy = x
        s = copy(1)
    end function copied_total
end module continue_compilation_traits_10_child

module continue_compilation_traits_10_counted
    use continue_compilation_traits_10_traits, only: INumeric
    use continue_compilation_traits_10_base, only: Base
    implicit none
    private
    public :: Counted

    ! Overrides a binding without an open slot.
    type, extends(Base) :: Counted
    contains
        procedure :: count => counted_count
    end type Counted

contains

    function counted_count{INumeric :: T}(self, x) result(n)
        class(Counted), intent(in) :: self
        type(T), intent(in) :: x(:)
        integer :: n
        n = size(x)
    end function counted_count
end module continue_compilation_traits_10_counted
