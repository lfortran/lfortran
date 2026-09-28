module assymed_rank_23_m
    implicit none
    interface
        subroutine s(a)
            type(*), dimension(..), intent(in) :: a
        end subroutine
    end interface
end module assumed_rank_23_m

program assumed_rank_23
    use assymed_rank_23_m
    implicit none
    integer :: x(2)
    x = 1
    call s(x(1))
end program assumed_rank_23

subroutine s(a)
    type(*), dimension(..), intent(in) :: a
    if (rank(a) /= 0) error stop
end subroutine s
