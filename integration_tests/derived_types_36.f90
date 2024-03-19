module derived_types_36_my_mod

    type :: my_type
        sequence
        character(len=:), allocatable :: a
    end type

    interface write(formatted)
        module procedure :: write_formatted
    end interface

    contains

    subroutine write_formatted(mt, unit, iotype, v_list, iostat, iomsg)
        type(my_type), intent(in) :: mt
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        write(unit, '(a)') mt%a
    end subroutine

end module

program derived_types_36
    use derived_types_36_my_mod, only: my_type, write(formatted)
    implicit none

    type(my_type) :: x
    integer :: iostat
    character(len=20) :: iomsg, tmp

    x%a = "tmp234"
    tmp = "tmp1"
    print *, tmp

    open(10, form="formatted", file="derived_types_36_file.txt")
    write(10, '(dt)', iostat=iostat, iomsg=iomsg) x
    close(10)

    open(10, form="formatted", file="derived_types_36_file.txt")
    read(10, '(a)', iostat=iostat, iomsg=iomsg) tmp
    close(10)

    print *, tmp
    if (tmp /= "tmp234") error stop

end program
