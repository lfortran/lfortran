module array_bounds_check_27_m
    implicit none
    type :: edge_t
        integer :: ivertex = 0
        character(len=:), allocatable :: label
    end type
contains
    impure elemental function ctor(ivertex, label) result(e)
        integer, intent(in), optional :: ivertex
        character(len=*), intent(in), optional :: label
        type(edge_t) :: e
        if (present(ivertex)) e%ivertex = ivertex
        if (present(label)) e%label = label
    end function

    subroutine set(edges, label, r)
        integer, intent(in) :: edges(:)
        character(len=*), intent(in), optional :: label(:)
        type(edge_t), allocatable, intent(out) :: r(:)
        ! every dummy of ctor is optional, so label must be present
        r = ctor(ivertex=edges, label=label)
    end subroutine
end module

program array_bounds_check_27
    use array_bounds_check_27_m
    implicit none
    type(edge_t), allocatable :: r(:)
    call set([5, 1], r=r)
    print *, "not reached"
end program
