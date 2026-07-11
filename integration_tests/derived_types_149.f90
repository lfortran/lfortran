module derived_types_149_mod
    implicit none

    type :: tensor_t
        integer :: cells_(2)
        integer :: order_
    end type

    type, extends(tensor_t) :: divergence_t
    end type

contains

    subroutine construct(d, cells, order)
        type(divergence_t), intent(out) :: d
        integer, intent(in) :: cells(2)
        integer, intent(in) :: order
        ! Assign to components accessed through the parent-type-name
        ! component (`d%tensor_t%...`) of an extended derived type.
        d%tensor_t%cells_ = cells
        d%tensor_t%order_ = order
    end subroutine

    integer function get_order(d) result(order)
        type(divergence_t), intent(in) :: d
        ! Read a component through the parent-type-name component.
        order = d%tensor_t%order_
    end function

end module derived_types_149_mod

program derived_types_149
    use derived_types_149_mod
    implicit none
    type(divergence_t) :: d

    call construct(d, [3, 4], 5)

    if (d%cells_(1) /= 3) error stop
    if (d%cells_(2) /= 4) error stop
    if (d%order_ /= 5) error stop
    if (get_order(d) /= 5) error stop

    print *, d%cells_, d%order_
end program derived_types_149
