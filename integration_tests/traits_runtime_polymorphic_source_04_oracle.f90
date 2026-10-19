! The standard Fortran counterpart of traits_runtime_polymorphic_source_04,
! with an abstract type in place of the trait. A generic with specifics for
! both the abstract type and its extension would be ambiguous here.
module traits_runtime_polymorphic_source_04_oracle_m
    implicit none
    type, abstract :: ICount
    contains
        procedure(get_i), deferred :: get
        procedure(bump_i), deferred :: bump
    end type
    abstract interface
        integer function get_i(self)
            import ICount
            class(ICount), intent(in) :: self
        end function
        subroutine bump_i(self)
            import ICount
            class(ICount), intent(inout) :: self
        end subroutine
    end interface
    type, extends(ICount) :: A
        integer :: n = 0
    contains
        procedure :: get => a_get
        procedure :: bump => a_bump
    end type
    type, extends(A) :: B
    contains
        procedure :: get => b_get
    end type
    type :: Wrapper
        class(ICount), allocatable :: item
        integer :: made_by = 0
    end type
    interface Wrapper
        module procedure make_wrapper
    end interface
    interface observe
        module procedure observe_view
    end interface
    interface advance
        module procedure advance_view
    end interface
contains
    integer function a_get(self)
        class(A), intent(in) :: self
        a_get = self%n
    end function
    subroutine a_bump(self)
        class(A), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    integer function b_get(self)
        class(B), intent(in) :: self
        b_get = 100 + self%n
    end function
    function make_wrapper(item) result(res)
        class(ICount), intent(in) :: item
        type(Wrapper) :: res
        res%item = item
        res%made_by = 1
    end function
    integer function observe_view(view)
        class(ICount), intent(in) :: view
        observe_view = view%get()
    end function
    subroutine advance_view(view)
        class(ICount), intent(inout) :: view
        call view%bump()
    end subroutine
end module

program traits_runtime_polymorphic_source_04_oracle
    use traits_runtime_polymorphic_source_04_oracle_m
    implicit none
    class(A), allocatable :: poly
    type(Wrapper) :: w
    allocate(B :: poly)
    poly%n = 3
    if (observe(poly) /= 103) error stop 1
    call advance(poly)
    if (poly%n /= 4 .or. observe(poly) /= 104) error stop 2
    w = Wrapper(poly)
    if (w%made_by /= 1 .or. w%item%get() /= 104) error stop 3
    print '(a)', 'traits_runtime_polymorphic_source_04_oracle ok'
end program
