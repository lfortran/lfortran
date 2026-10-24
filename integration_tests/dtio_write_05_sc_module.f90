! Types for dtio_write_05_sc: a binding named differently from its procedure,
! inherited by t2 and overridden by t3 in a second module.
module dtio_write_05_sc_base
implicit none
integer :: out = -1

type :: t
    integer :: i = 0
contains
    procedure :: write_me => wf_impl
    generic :: write(formatted) => write_me
end type

contains

subroutine wf_impl(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("<t:",i0,">")', iostat=iostat) dtv%i
end subroutine

end module

module dtio_write_05_sc_ext
use dtio_write_05_sc_base
implicit none

type, extends(t) :: t2
    integer :: j = 0
end type

type, extends(t) :: t3
    integer :: k = 0
contains
    procedure :: write_me => wf3_impl
end type

contains

subroutine wf3_impl(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t3), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("<t3:",i0,",",i0,">")', iostat=iostat) dtv%i, dtv%k
end subroutine

subroutine emit(y, z, c)
    type(t2), intent(in) :: y
    type(t3), intent(in) :: z
    class(t), intent(in) :: c
    write(out, '(dt,1x,dt,1x,dt)') y, z, c
end subroutine

end module
