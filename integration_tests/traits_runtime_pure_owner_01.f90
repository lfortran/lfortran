! A pure message of a trait implemented by a type that owns runtime trait
! objects, as the reported `MyType` with the component
! `class(IOtherTrait), allocatable :: obj` implements the pure function
! `method` of `ISomeTrait`. The witness adapter recovers the receiver from
! its view in place, which has no lifecycle effects, so the adapter stays
! pure: the message dispatches through views, owners and pure procedures.
module traits_runtime_pure_owner_01_m
    implicit none
    private
    public :: ISomeTrait, IOtherTrait, MyType, Many, Other, sum_of, peek

    abstract interface :: ISomeTrait
        pure function method(self) result(m)
            type(itself), pass, intent(in) :: self
            integer :: m
        end function method
    end interface

    abstract interface :: IOtherTrait
        pure function value(self) result(v)
            type(itself), pass, intent(in) :: self
            integer :: v
        end function value
    end interface

    type, implements(IOtherTrait) :: Other
        integer :: v = 0
    contains
        procedure :: value => other_value
    end type Other

    type, implements(ISomeTrait) :: MyType
        class(IOtherTrait), allocatable :: obj
    contains
        procedure :: method
    end type MyType

    ! An implementation that owns a runtime trait array.
    type, implements(ISomeTrait) :: Many
        class(IOtherTrait), allocatable :: arr(:)
    contains
        procedure :: method => many_method
    end type Many

contains

    pure function other_value(self) result(v)
        class(Other), intent(in) :: self
        integer :: v
        v = self%v
    end function other_value

    pure function method(self) result(m)
        class(MyType), intent(in) :: self
        integer :: m
        m = -1
        if (allocated(self%obj)) m = self%obj%value()
    end function method

    pure function many_method(self) result(m)
        class(Many), intent(in) :: self
        integer :: m, i
        m = 0
        if (.not. allocated(self%arr)) return
        do i = 1, size(self%arr)
            m = m + self%arr(i)%value()
        end do
    end function many_method

    pure integer function peek(x)
        class(ISomeTrait), intent(in) :: x
        peek = x%method()
    end function peek

    pure integer function sum_of(a, b)
        class(ISomeTrait), intent(in) :: a, b
        sum_of = a%method() + b%method()
    end function sum_of
end module traits_runtime_pure_owner_01_m

program traits_runtime_pure_owner_01
    use traits_runtime_pure_owner_01_m
    implicit none
    type(MyType) :: t
    type(Many) :: n
    class(ISomeTrait), allocatable :: s

    if (t%method() /= -1 .or. peek(t) /= -1) error stop 1
    allocate(t%obj, source=Other(5))
    if (t%method() /= 5 .or. peek(t) /= 5) error stop 2
    allocate(n%arr(3), source=Other(4))
    if (peek(n) /= 12 .or. sum_of(t, n) /= 17) error stop 3
    s = t
    if (s%method() /= 5 .or. peek(s) /= 5) error stop 4
    deallocate(t%obj)
    if (s%method() /= 5 .or. peek(t) /= -1) error stop 5
    s = n
    if (s%method() /= 12 .or. sum_of(s, t) /= 11) error stop 6
    print *, "ok"
end program traits_runtime_pure_owner_01
