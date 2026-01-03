module write_10_m
    type :: mytype
        character(len=:), allocatable :: raw
    end type

    interface write(unformatted)
        module procedure :: my_write
    end interface

    interface read(unformatted)
        module procedure :: my_read
    end interface

contains

    subroutine my_write(x, unit, iostat, iomsg)
        class(mytype), intent(in) :: x
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        write(unit, iostat=iostat, iomsg=iomsg) len(x%raw)
        print *, "Writing length:", len(x%raw)
        if (len(x%raw) /= 5) error stop
    end subroutine

    subroutine my_read(x, unit, iostat, iomsg)
        class(mytype), intent(inout) :: x
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        integer :: l
        read(unit, iostat=iostat, iomsg=iomsg) l
        allocate(character(len=l) :: x%raw)
    end subroutine

end module write_10_m

program write_10
    use write_10_m
    implicit none
    type(mytype) :: x, y
    integer :: u, iostat
    character(len=100) :: iomsg

    x%raw = "hello"

    open(newunit=u, form="unformatted", status="scratch")
    write(u, iostat=iostat, iomsg=iomsg) x
    print *, len(x%raw)
    if (len(x%raw) /= 5) error stop
    rewind(u)
    read(u, iostat=iostat, iomsg=iomsg) y
    print *, len(y%raw)
    if (len(y%raw) /= 5) error stop
    close(u)
end program write_10

