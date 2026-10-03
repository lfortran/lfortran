program select_rank_21
    implicit none
    real, allocatable :: arr(:)
    call alloc_sub(arr)
    if (size(arr) /= 10) error stop
    if (arr(1) /= 0.0) error stop

contains

    subroutine alloc_sub(x)
        real, allocatable, dimension(..), intent(out) :: x
        select rank(x)
        rank(1)
            allocate(x(10))
            x = 0.0
        rank(2)
            allocate(x(3, 4))
        rank default
            error stop "unexpected rank"
        end select
    end subroutine

end program select_rank_21
