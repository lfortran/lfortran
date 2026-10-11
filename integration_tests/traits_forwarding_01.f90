! LFortran trait extension: four-level forwarding with reversed nominal binders.
module traits_forwarding_01_m
    implicit none
    abstract interface :: IBase
        function value() result(r)
            integer :: r
        end function
    end interface
    abstract interface, extends(IBase) :: IChild
    end interface
    type :: Box
        integer :: data
    end type
    type :: Scaled
        integer :: data
    end type
    implements IChild :: Box
        procedure, pass :: value => box_value
    end implements
    implements IChild :: Scaled
        procedure, pass :: value => scaled_value
    end implements
contains
    function top{IChild :: T, IChild :: U}(a, b) result(r)
        type(T), intent(in) :: a
        type(U), intent(in) :: b
        integer :: r
        r = middle(a, b) + 100 * middle{U, T}(b, a)
    end function
    function middle{IChild :: A, IBase :: B}(x, y) result(r)
        type(A), intent(in) :: x
        type(B), intent(in) :: y
        integer :: r
        r = pair(x, y) + 10 * pair{B, A}(y, x)
    end function
    function pair{IBase :: X, IBase :: Y}(a, b) result(r)
        type(X), intent(in) :: a
        type(Y), intent(in) :: b
        integer :: r
        r = 1000 * read_value(a) + read_value(b)
    end function
    function read_value{IBase :: V}(x) result(r)
        type(V), intent(in) :: x
        integer :: r
        r = x%value()
    end function
    function box_value(self) result(r)
        class(Box), intent(in) :: self
        integer :: r
        r = self%data
    end function
    function scaled_value(self) result(r)
        class(Scaled), intent(in) :: self
        integer :: r
        r = 10 + self%data
    end function
end module
program traits_forwarding_01
    use traits_forwarding_01_m
    implicit none
    type(Box) :: a
    type(Scaled) :: b
    integer :: expected, reversed
    a = Box(2)
    b = Scaled(7)
    expected = (1000*2+17) + 10*(1000*17+2)
    reversed = (1000*17+2) + 10*(1000*2+17)
    if (top(a, b) /= expected + 100*reversed) error stop 1
    if (top(b, a) /= reversed + 100*expected) error stop 2
    if (top{Box, Scaled}(a, b) /= expected + 100*reversed) error stop 3
end program
