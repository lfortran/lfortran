module derived_types_34_my_mod

    type :: my_type
        sequence
        integer :: a
    end type

    interface read(formatted)
        module procedure :: read_formatted
    end interface

    contains

    subroutine read_formatted(mt, unit, iotype, v_list, iostat, iomsg)
        type(my_type), intent(inout) :: mt
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        mt%a = 125
    end subroutine

end module

program derived_types_34
    use derived_types_34_my_mod, only: my_type, read(formatted)
    implicit none

    print *, "Ok"
end program
