! Defined output of the elements of a polymorphic array, as an array item.
module dtio_write_01_mod
implicit none
type :: t
    integer :: v = 3
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
    write(unit, '(a,i0)', iostat=iostat) 'T', dtv%v
end subroutine
end module

program dtio_write_01
use dtio_write_01_mod
implicit none
class(t), allocatable :: c(:)
integer :: i
allocate(c(2))
print '(2(dt,1x))', c
end program
