! Extended types inherit the type-bound write(formatted) of their parent.
! One overrides its specific binding, the other does not.

module write_55_mod
    implicit none
    character(5) :: called = ''

    type :: base_t
        integer :: i = 0
    contains
        procedure :: w
        generic :: write(formatted) => w
    end type

    type, extends(base_t) :: child_t
    contains
        procedure :: w => w_child
    end type

    type, extends(base_t) :: other_t
    end type

contains

    subroutine w(dtv, unit, iotype, v_list, iostat, iomsg)
        class(base_t), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        called = 'base'
        write(unit, '(a, i0)', iostat=iostat) 'base', dtv%i
    end subroutine

    subroutine w_child(dtv, unit, iotype, v_list, iostat, iomsg)
        class(child_t), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        called = 'child'
        write(unit, '(a, i0)', iostat=iostat) 'child', dtv%i
    end subroutine

end module

program write_55
    use write_55_mod
    implicit none
    type(child_t) :: c
    type(other_t) :: o

    c%i = 1
    o%i = 2
    write(*, '(DT)') c
    if (called /= 'child') error stop
    write(*, '(DT)') o
    if (called /= 'base') error stop
end program
