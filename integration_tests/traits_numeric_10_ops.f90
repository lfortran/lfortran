module traits_numeric_10_ops_m
    implicit none
    private
    public :: shift
    integer, parameter :: rk = 8
contains
    function shift{integer | real(rk) :: T}(x, n) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        type(T) :: r
        r = x + T(n)
    end function
end module

module traits_numeric_10_other_m
    implicit none
    private
    public :: shift
contains
    function shift{integer | real(8) :: T}(x, n) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        type(T) :: r
        r = x + T(n) + T(n)
    end function
end module
