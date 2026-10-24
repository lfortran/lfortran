module dtio_write_01_sc_types
implicit none
integer :: nunit = 0, out = -1

type :: t
    integer :: v = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

type :: s
    integer :: v = 0
end type

interface write(formatted)
    module procedure ws
end interface

contains

subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("<",a,":",i0,">")', iostat=iostat) iotype, dtv%v
end subroutine

subroutine ws(dtv, unit, iotype, v_list, iostat, iomsg)
    class(s), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("{",i0,"}")', iostat=iostat) dtv%v
end subroutine

integer function unit_fn()
    nunit = nunit + 1
    unit_fn = out
end function

end module

module dtio_write_01_sc_io
use dtio_write_01_sc_types, only: t, s, write(formatted), out, unit_fn
implicit none
contains

subroutine emit(y, w)
    type(t), intent(in) :: y
    type(s), intent(in) :: w
    write(out, '(dt,1x,dt)') y, w
    write(unit_fn(), '(i0,1x,dt)') 3, y
end subroutine

end module
