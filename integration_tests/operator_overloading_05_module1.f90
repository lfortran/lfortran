module operator_overloading_05_module1
    type :: string_type
        character(len=:), allocatable :: raw
    end type string_type

    interface assignment(=)
        module procedure :: assign_string_type
    end interface assignment(=)

    interface operator(>=)
        module procedure :: gt_string_type
    end interface operator(>=)

contains

    pure subroutine assign_string_type(lhs, rhs)
        type(string_type), intent(inout) :: lhs
        type(string_type), intent(in) :: rhs
        lhs%raw = rhs%raw
    end subroutine assign_string_type

    pure function gt_string_type(lhs, rhs) result(r)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: r
        r = .true.
    end function gt_string_type

end module