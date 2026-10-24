! Types for dtio_write_16. With the external procedures s_dtio16 and dtio16,
! the binding names wf and wf_s would give the same callback name if a callback
! were named only by its binding and procedure ("wf" + "s_dtio16" and
! "wf_s" + "dtio16").
module dtio_write_16_mod
implicit none

type :: t
    integer :: v = 1
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

type :: u
    integer :: v = 2
contains
    procedure :: wf_s => wfu
    generic :: write(formatted) => wf_s
end type

contains

subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("T",i0)', iostat=iostat) dtv%v
end subroutine

subroutine wfu(dtv, unit, iotype, v_list, iostat, iomsg)
    class(u), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("U",i0)', iostat=iostat) dtv%v
end subroutine

end module
