module dtio_write_18_mod
    implicit none
    private
    public :: string_wrapper
    public :: write(formatted)

    type :: string_wrapper
        character(len=:), allocatable :: raw
    end type string_wrapper

    interface write(formatted)
        module procedure :: write_formatted
    end interface

contains

    subroutine write_formatted(strwrap, unit, iotype, v_list, iostat, iomsg)
        class(string_wrapper), intent(in) :: strwrap
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        select case(iotype)
        case("LISTDIRECTED")
            write(unit, '(a)', iostat=iostat, iomsg=iomsg) maybe(strwrap)
        case("NAMELIST")
            error stop "[Fatal] NAMELIST output not supported."
        case default
            select case(size(v_list))
            case(0)
                write(unit, '(a)', iostat=iostat, iomsg=iomsg) maybe(strwrap)
            case default
                error stop "[Fatal] v_list formatting not supported."
            end select
        end select
    end subroutine write_formatted

    pure function maybe(strwrap) result(result_string)
        type(string_wrapper), intent(in) :: strwrap
        character(len=:), allocatable :: result_string
        if (allocated(strwrap%raw)) then
            result_string = strwrap%raw
        else
            result_string = ''
        end if
    end function maybe

end module dtio_write_18_mod

program dtio_write_18
    ! list-directed write of a type with a generic write(formatted)
    ! interface (stdlib string_type pattern) to a scratch file
    use dtio_write_18_mod
    implicit none

    type(string_wrapper) :: str
    integer :: io
    character(len=100) :: buffer, line

    str%raw = "Important saved value"

    open(newunit=io, form="formatted", status="scratch")
    write(io, *) str
    rewind(io)
    read(io, *) buffer
    rewind(io)
    read(io, '(a)') line
    close(io)

    print *, "Read back:", trim(buffer)
    if (trim(buffer) /= "Important") error stop
    if (trim(adjustl(line)) /= "Important saved value") error stop
end program dtio_write_18
