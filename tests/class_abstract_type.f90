module class_abstract_type_module

    implicit none

    type :: other_type
        class(*), allocatable :: value
    end type other_type

end module class_abstract_type_module

program class_abstract_type

    use class_abstract_type_module
    implicit none

    type(other_type) :: other
    call get_other_open_data( other)

contains

    subroutine get_other_open_data( other )
        type(other_type), intent(out) :: other
    end subroutine get_other_open_data

end program class_abstract_type