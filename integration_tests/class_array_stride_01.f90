module class_array_stride_01_m
    implicit none
    type :: node_t
        integer :: first = 0, width = 0, m = 0
        integer, allocatable :: rows(:)
    end type node_t
    type, extends(node_t) :: panel_t
        integer :: update_first = 0, update_last = -1
        real(8), pointer, contiguous :: values(:, :) => null()
    end type panel_t
contains
    integer function total(nodes) result(t)
        class(node_t), intent(in) :: nodes(:)
        integer :: s
        t = 0
        do s = 1, size(nodes)
            t = t + 1000*nodes(s)%width + nodes(s)%rows(nodes(s)%m)
        end do
    end function total
end module class_array_stride_01_m

program class_array_stride_01
    ! Elements of a class(node_t) array dummy holding extended types are
    ! addressed with the size of the dynamic type, also for wasm32.
    use class_array_stride_01_m
    implicit none
    type(panel_t), allocatable :: panels(:)
    integer :: s
    allocate(panels(3))
    do s = 1, 3
        panels(s)%width = s
        panels(s)%m = s + 1
        allocate(panels(s)%rows(s + 1))
        panels(s)%rows = 10*s
    end do
    print *, total(panels)
    if (total(panels) /= 6060) error stop 1
end program class_array_stride_01
