module traits_runtime_combination_01_alternative_m
    use traits_runtime_combination_01_contracts_m, only: IRich, PublicBox
    implicit none
    private
    implements IRich :: PublicBox
        procedure, pass :: value => alternate_value
        procedure, pass(self) :: scaled => alternate_scaled
        procedure, nopass :: label => alternate_label
    end implements
contains
    integer function alternate_value(self)
        type(PublicBox), intent(in) :: self
        alternate_value = 1000 + self%payload
    end function
    integer function alternate_scaled(factor, self)
        integer, intent(in) :: factor
        type(PublicBox), intent(in) :: self
        alternate_scaled = 2000 + factor * self%payload
    end function
    integer function alternate_label()
        alternate_label = 909
    end function
end module
