module wrappers_derived_types_83
    implicit none

    type :: other_type
        class(*), allocatable :: value
    end type other_type

    interface set
        module procedure set_other
    end interface set

contains

    subroutine set_other( other, value )
        type(other_type), intent(out) :: other
        class(*), intent(in)          :: value
        allocate(other % value, source=value)
    end subroutine set_other

end module wrappers_derived_types_83


program derived_types_83

    use wrappers_derived_types_83
    implicit none
    
contains

    subroutine input_random_data( )
        class(*), allocatable :: dummy
        type(other_type) :: other
        call set ( other, dummy )
    end subroutine input_random_data

end program