module module_nullify_04
    implicit none

    type :: scaled_data_structure
        real, dimension(:), pointer :: one_d_real_pointer
    end type
end module

program nullify_04
    use module_nullify_04, only : scaled_data_structure
    implicit none
    type(scaled_data_structure) :: s
    type(scaled_data_structure) :: s2
    nullify(s%one_d_real_pointer)
    nullify(s2%one_d_real_pointer)
end program