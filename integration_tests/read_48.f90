module read_48_m
    implicit none
    type :: t
        integer :: x
    end type
    interface read(unformatted)
        module procedure :: read_unf
    end interface
contains
    subroutine read_unf(dt, unit, iostat, iomsg)
        class(t), intent(inout) :: dt
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        read(unit, iostat=iostat, iomsg=iomsg) dt%x
    end subroutine
end module

program read_48
    use read_48_m
    implicit none
    type(t) :: s
    integer :: io
    open(newunit=io, form="unformatted", status="scratch")
    write(io) 42
    rewind(io)
    read(io) s
    close(io)
    print *, s%x
    if (s%x /= 42) error stop
end program
