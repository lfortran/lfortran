module traits_runtime_05_alternative_m
    use traits_runtime_05_contracts_m, only: ICombined, Box
    implicit none
    private
    implements ICombined :: Box
        procedure, pass :: value => alternate_value
        procedure, nopass :: label => alternate_label
        procedure, pass :: double_value => alternate_double
    end implements
contains
    integer function alternate_value(self)
        type(Box), intent(in) :: self
        alternate_value = 1000 + self%payload
    end function
    integer function alternate_double(self)
        type(Box), intent(in) :: self
        alternate_double = 2000 + 2 * self%payload
    end function
    integer function alternate_label()
        alternate_label = 909
    end function
end module
