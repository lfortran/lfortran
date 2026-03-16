program pure_side_effects_02
    implicit none
    integer, allocatable :: a(:), b(:)

    allocate(a(3))
    a = [1, 2, 3]
    call pure_move(a, b)
    if (size(b) /= 3) error stop
    if (b(1) /= 1) error stop
    if (b(2) /= 2) error stop
    if (b(3) /= 3) error stop
    if (allocated(a)) error stop

contains

    pure subroutine pure_move(src, dst)
        integer, allocatable, intent(inout) :: src(:)
        integer, allocatable, intent(out) :: dst(:)
        call move_alloc(src, dst)
    end subroutine pure_move

end program pure_side_effects_02
