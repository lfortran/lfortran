module traits_runtime_inspection_alternative_m
    use traits_runtime_inspection_contracts_m, only: IRich, PublicCell
    implicit none
    implements IRich :: PublicCell
        procedure, pass :: value => alternative_value
        procedure, nopass :: label => alternative_label
    end implements
contains
    integer function alternative_value(self)
        type(PublicCell), intent(in) :: self
        alternative_value = self%n + 1000
    end function
    integer function alternative_label()
        alternative_label = 303
    end function
end module
