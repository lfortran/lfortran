! Static traits are an LFortran extension; see traits_recursive_01_oracle.f90.
module traits_recursive_01_m
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
    type :: OffsetBox
        integer :: data
    end type
    implements IChild :: Box
        procedure, pass :: value => box_value
    end implements
    implements IChild :: OffsetBox
        procedure, pass :: value => offset_value
    end implements
contains
    function box_value(self) result(r)
        class(Box), intent(in) :: self
        integer :: r
        r = self%data
    end function
    function offset_value(self) result(r)
        class(OffsetBox), intent(in) :: self
        integer :: r
        r = self%data + 100
    end function
    recursive function first{IBase :: T}(x, n) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        integer :: r
        if (n == 0) then
            r = x%value()
        else
            r = second(x, n-1) + 1
        end if
    end function
    recursive function second{IBase :: U}(x, n) result(r)
        type(U), intent(in) :: x
        integer, intent(in) :: n
        integer :: r
        if (n == 0) then
            r = x%value()
        else
            r = first{U}(x, n-1) + 1
        end if
    end function
    recursive function total{IBase :: T}(x, n) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        integer :: r
        if (n == 0) then
            r = x%value()
        else
            r = total(x, n-1) + 1
        end if
    end function
    subroutine empty{IBase :: T}(x)
        type(T), intent(in) :: x
    end subroutine
end module
