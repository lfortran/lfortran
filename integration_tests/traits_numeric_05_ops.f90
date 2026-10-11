module traits_numeric_05_ops_m
    use traits_numeric_05_contracts_m, only: RenamedNumeric => INumeric
    use traits_numeric_05_other_m, only: IntegerOnly => INumeric
    implicit none
    private
    public :: RenamedNumeric, IntegerOnly, bump, integer_bump

contains

    function bump{RenamedNumeric :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x + T(1)
    end function bump

    function integer_bump{IntegerOnly :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x + T(2)
    end function integer_bump
end module traits_numeric_05_ops_m
