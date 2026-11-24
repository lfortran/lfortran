! access statements naming the defined input/output generics: read(formatted)
! and write(formatted), in upper case and without "::"
module dtio_write_19_mod
    implicit none
    private
    PUBLIC :: t, READ(FORMATTED)
    public write(Formatted)
    type :: t
        integer :: i = 0
    end type
    interface read(formatted)
        module procedure :: rd
    end interface
    interface write(formatted)
        module procedure :: wr
    end interface
contains
    subroutine rd(dtv, unit, iotype, v_list, iostat, iomsg)
        class(t), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        read(unit, *, iostat=iostat, iomsg=iomsg) dtv%i
        dtv%i = dtv%i + 1
    end subroutine
    subroutine wr(dtv, unit, iotype, v_list, iostat, iomsg)
        class(t), intent(in) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        write(unit, '(i0)', iostat=iostat, iomsg=iomsg) 10*dtv%i
    end subroutine
end module dtio_write_19_mod

program dtio_write_19
    use dtio_write_19_mod
    implicit none
    type(t) :: x
    integer :: io
    character(len=20) :: line
    open(newunit=io, status="scratch", form="formatted")
    write(io, *) "41"
    rewind(io)
    read(io, *) x
    close(io)
    print *, x%i
    if (x%i /= 42) error stop
    open(newunit=io, status="scratch", form="formatted")
    write(io, '(dt)') x
    rewind(io)
    read(io, '(a)') line
    close(io)
    print *, trim(line)
    if (trim(adjustl(line)) /= "420") error stop
end program dtio_write_19
