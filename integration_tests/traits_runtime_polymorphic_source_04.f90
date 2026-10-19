! Generic resolution accepts a class(A) actual for a trait view dummy when the
! declared type A conforms (TD-38): through function and subroutine generics,
! intent(inout) views and a generic named like a type, which is then not its
! structure constructor. A specific taking class(A) itself still wins.
module traits_runtime_polymorphic_source_04_m
    implicit none
    abstract interface :: ICount
        integer function get(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine bump(self)
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
    type, implements(ICount) :: A
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
    interface pick
        module procedure pick_view, pick_class
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
    integer function pick_view(view)
        class(ICount), intent(in) :: view
        pick_view = 1
    end function
    integer function pick_class(object)
        class(A), intent(in) :: object
        pick_class = 2
    end function
end module

program traits_runtime_polymorphic_source_04
    use traits_runtime_polymorphic_source_04_m
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
    if (pick(poly) /= 2 .or. pick(A(1)) /= 2) error stop 4
    print '(a)', 'traits_runtime_polymorphic_source_04 ok'
end program
