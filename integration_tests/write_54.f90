! A type-bound write(formatted) whose binding name differs from the name of
! its procedure.

module write_54_mod
    implicit none
    integer :: calls = 0

    type :: t
        integer :: i = 0
    contains
        procedure :: wb => w
        generic :: write(formatted) => wb
    end type

contains

    subroutine w(dtv, unit, iotype, v_list, iostat, iomsg)
        class(t), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        calls = calls + 1
        write(unit, '(a, i0)', iostat=iostat) 'w', dtv%i
    end subroutine

end module

program write_54
    use write_54_mod
    implicit none
    type(t) :: x

    x%i = 3
    write(*, '(DT)') x
    if (calls /= 1) error stop
end program
