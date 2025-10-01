program assumed_rank_01
    implicit none
    integer :: a(3) = [1,2,3]
    integer :: b(2, 2, 1) = reshape([1, 2, 3, 4], [2, 2, 1])
    call show(a, 3, 1)
    call show(b, 4, 3)
contains
    subroutine show(x, expected_size, expected_rank)
        integer :: x(..)
        integer, intent(in) :: expected_size, expected_rank
        if (size(x) /= expected_size) error stop
        if (rank(x) /= expected_rank) error stop
        print *, size(x)
        print *, rank(x)
    end subroutine
end program 