program assumed_rank_24
    implicit none
    interface
        subroutine s(a)
            type(*), dimension(..), intent(in) :: a
        end subroutine
    end interface
    integer :: x(2)
    x = 1
    call s(x(1))
end program assumed_rank_24

subroutine s(a)
    type(*), dimension(..), intent(in) :: a
    if (rank(a) /= 0) error stop
end subroutine
