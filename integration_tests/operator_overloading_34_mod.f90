module operator_overloading_34_mod
    implicit none
    type :: array_type
        real, allocatable :: val(:,:)
    end type
    interface operator(.gt.)
        module procedure gt_scalar
    end interface

contains

    function gt_scalar(a, b) result(c)
        type(array_type), intent(in) :: a
        real, intent(in) :: b
        logical, dimension(size(a%val,1), size(a%val,2)) :: c
        c = a%val > b
    end function

end module
