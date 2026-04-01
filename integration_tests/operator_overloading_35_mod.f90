module operator_overloading_35_types
    implicit none
    type :: vec_t
        real :: x
    contains
        procedure :: sub_vec
        procedure :: neg_vec
        generic :: operator(-) => sub_vec, neg_vec
    end type
    interface operator(-)
        module procedure scalar_sub_vec
    end interface
contains
    function sub_vec(a, b) result(c)
        class(vec_t), intent(in) :: a, b
        type(vec_t) :: c
        c%x = a%x - b%x
    end function
    function neg_vec(a) result(c)
        class(vec_t), intent(in) :: a
        type(vec_t) :: c
        c%x = -a%x
    end function
    function scalar_sub_vec(s, b) result(c)
        real, intent(in) :: s
        type(vec_t), intent(in) :: b
        type(vec_t) :: c
        c%x = s - b%x
    end function
end module

module operator_overloading_35_reexport
    use operator_overloading_35_types, only: vec_t, operator(-)
    implicit none
    public :: vec_t, operator(-)
end module
