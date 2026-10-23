module dtio_write_10_ma
implicit none
type :: t
    integer :: v = 1
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type
contains
subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("A",i0)', iostat=iostat) dtv%v
end subroutine
end module
