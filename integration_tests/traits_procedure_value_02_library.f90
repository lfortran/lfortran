module traits_procedure_value_02_library
    implicit none
    private
    public :: average
    abstract interface :: INumeric
        integer | real(8)
    end interface
contains
    function average{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = pairwise(x) / T(size(x))
    end function
    function pairwise{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: middle
        if (size(x) == 1) then
            s = x(1)
        else
            middle = size(x) / 2
            s = pairwise(x(:middle)) + pairwise(x(middle+1:))
        end if
    end function
end module
