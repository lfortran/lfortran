module dtio_write_17_module
implicit none
contains
recursive subroutine host(depth)
    integer, intent(in) :: depth
    type :: item
        integer :: value
    end type
    interface write(formatted)
        procedure write_item
    end interface
    integer :: captured, calls
    character(32) :: text, expected
    captured = depth * 10
    calls = 0
    ! GFortran 15 incorrectly saves local DTIO objects (GCC PR125059).
    ! Fresh operands leave captured and calls as the state tested across recursion.
    write(text, '(dt)') item(depth)
    write(expected, '(i0)') depth * 11
    if (text /= expected .or. calls /= 1) error stop 1
    if (depth > 1) call host(depth - 1)
    captured = captured + 1
    write(text, '(dt)') item(depth)
    write(expected, '(i0)') depth * 11 + 1
    if (text /= expected .or. calls /= 2) error stop 2
contains
    subroutine write_item(dtv, unit, iotype, v_list, iostat, iomsg)
        class(item), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        calls = calls + 1
        write(unit, '(i0)', iostat=iostat, iomsg=iomsg) captured + dtv%value
    end subroutine
end subroutine
end module
program dtio_write_17
use dtio_write_17_module
implicit none
call host(3)
end program
