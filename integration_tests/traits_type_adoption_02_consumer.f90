module traits_type_adoption_02_consumer
    use traits_type_adoption_02_contracts, only: IAll, IValue, IExtra
    implicit none
contains
    integer function static_read{IAll :: T}(x) result(n)
        type(T), intent(in) :: x
        n = x%value() + x%extra(2) + int(x%measure())
    end function
    integer function read_subset(x) result(n)
        class(IValue + IExtra), intent(in) :: x
        n = x%value() + x%extra(2)
    end function
end module
