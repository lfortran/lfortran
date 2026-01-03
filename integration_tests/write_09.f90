module write_09_m
    type :: mytype
        character(len=:), allocatable :: raw
    end type

    interface write(unformatted)
        module procedure :: my_write
    end interface

contains

    subroutine my_write(x, unit, iostat, iomsg)
        class(mytype), intent(in) :: x
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        write(unit, iostat=iostat, iomsg=iomsg) len(x%raw)
    end subroutine
end module

program write_09
    use write_09_m
    implicit none
    type(mytype) :: x
    integer :: u, iostat
    character(len=100) :: iomsg

    x%raw = "hello"

    open(newunit=u, form="unformatted", status="scratch")
    write(u, iostat=iostat, iomsg=iomsg) x
    close(u)
end program write_09
