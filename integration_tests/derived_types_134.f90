module derived_types_134_mod
    implicit none

    type :: vertex_type
        integer :: degree = 0
    end type

    type :: graph_type
        type(vertex_type), dimension(:), allocatable :: vertex
        integer :: num_vertices = 0
    end type

contains
    subroutine calc_degree(this)
        type(graph_type), intent(inout) :: this
        integer :: i
        this%vertex(:)%degree = 0
        do i = 1, this%num_vertices
            this%vertex(i)%degree = i
        end do
    end subroutine
end module

program derived_types_134
    use derived_types_134_mod
    implicit none
    type(graph_type) :: g

    g%num_vertices = 3
    allocate(g%vertex(3))
    call calc_degree(g)

    if (g%vertex(1)%degree /= 1) error stop
    if (g%vertex(2)%degree /= 2) error stop
    if (g%vertex(3)%degree /= 3) error stop
    print *, "PASS"
end program
