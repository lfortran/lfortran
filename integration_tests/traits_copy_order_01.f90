! Traits are an LFortran extension; declaration_copy_01 is the standard oracle.
module traits_copy_order_01_m
    implicit none
    abstract interface :: ICount
        function count(n, a, btext) result(r)
            integer, intent(in) :: n, a(n)
            character(len=n), intent(in) :: btext
            integer :: r
        end function
    end interface
    type :: Box
        integer :: value
    end type
    implements ICount :: Box
        procedure, pass(self) :: count => box_count
    end implements
contains
    function box_count(extent, values, self, letters) result(r)
        integer, intent(in) :: extent, values(extent)
        class(Box), intent(in) :: self
        character(len=extent), intent(in) :: letters
        integer :: r
        r = self%value + sum(values) + len(letters)
    end function
    function unused{ICount :: T}(x) result(r)
        type(T), intent(in) :: x
        integer :: r
        r = 0
    end function
end module

program traits_copy_order_01
    use traits_copy_order_01_m
    implicit none
    type(Box) :: x
    x = Box(10)
    if (x%count(3, [1, 2, 3], "abc") /= 19) error stop 1
    if (x%count(1, [7], "q") /= 18) error stop 2
end program
