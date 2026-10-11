! Traits are an LFortran extension; GFortran does not accept this syntax.
module traits_composition_04_m
    implicit none

    abstract interface :: ILeft
        function count(a) result(r)
            integer, intent(in) :: a(:)
            integer :: r
        end function
    end interface
    abstract interface :: IRight
        function count(a) result(r)
            integer, intent(in) :: a(:)
            integer :: r
        end function
    end interface
    abstract interface, extends(ILeft + IRight) :: ICombined
    end interface

    type :: Payload
        integer :: offset
    end type
    implements ICombined :: Payload
        procedure, pass :: count => payload_count
    end implements

contains

    function payload_count(self, a) result(r)
        class(Payload), intent(in) :: self
        integer, intent(in) :: a(:)
        integer :: r
        r = self%offset + size(a) + sum(a)
    end function

    function left_first{ILeft + IRight :: T}(x, a) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: a(:)
        integer :: r
        r = x%count(a)
    end function

    function right_first{IRight + ILeft :: T}(x, a) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: a(:)
        integer :: r
        r = x%count(a)
    end function
end module

program traits_composition_04
    use traits_composition_04_m
    implicit none
    type(Payload) :: x
    integer :: a(6)
    x = Payload(11)
    a = [1, 2, 3, 4, 5, 6]
    if (left_first(x, a) /= 38) error stop 1
    if (right_first(x, a) /= 38) error stop 2
    if (left_first(x, a(1:6:2)) /= 23) error stop 3
    if (right_first(x, a(2:6:2)) /= 26) error stop 4
end program
