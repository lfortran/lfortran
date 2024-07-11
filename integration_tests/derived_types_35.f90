module derived_types_35_my_mod

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

        read(unit, *) mt%a
    end subroutine

end module

program derived_types_35
    use derived_types_35_my_mod, only: my_type, read(formatted)
    implicit none

    type(my_type) :: x
    integer :: io, iostat
    character(len=20) :: iomsg

    ! x%a = 10
    ! print *, x%a

    ! open(newunit=io, form="formatted", file="derived_types_35_file.txt")
    read(io, *, iostat=iostat, iomsg=iomsg) x
    ! close(io)

    ! print *, x%a
    ! if (x%a /= 125) error stop

end program
