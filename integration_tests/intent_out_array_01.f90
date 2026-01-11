! Test for intent(out) allocatable array of derived type with nested allocatables
! Issue #9097: nested allocatables must be deallocated before outer array on re-entry
program intent_out_array_01
    implicit none
    type :: node_t
        integer, allocatable :: data(:)
    end type
    type(node_t), allocatable :: nodes(:)

    call create_nodes(nodes, 3, 5)
    call create_nodes(nodes, 2, 4)  ! Re-entry: must dealloc nested first

    if (size(nodes) /= 2) error stop
    if (size(nodes(1)%data) /= 4) error stop
    print *, "PASS"
contains
    subroutine create_nodes(nodes, n, sz)
        type(node_t), allocatable, intent(out) :: nodes(:)
        integer, intent(in) :: n, sz
        integer :: i
        allocate(nodes(n))
        do i = 1, n
            allocate(nodes(i)%data(sz))
            nodes(i)%data = i * 10
        end do
    end subroutine
end program
