module assumed_rank_25_m
    implicit none
    type :: t
        character(len=8), allocatable :: names(:)
    end type
contains
    subroutine bcast(buf)
        type(*), dimension(..), intent(inout) :: buf
        if (rank(buf) /= 1) error stop
        if (size(buf) /= 3) error stop
    end subroutine
end module assumed_rank_25_m
program assumed_rank_25
    use assumed_rank_25_m
    implicit none
    type(t) :: x
    allocate(x%names(3))
    call bcast(x%names)
end program assumed_rank_25
