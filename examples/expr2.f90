program assumed_rank
    implicit none
    integer :: b(2, 2, 1) = reshape([1, 2, 3, 4], [2, 2, 1])
    integer :: a(3) = [1,2,3]
    call show(a)
    call show(b)
contains
    subroutine show(x)
        integer :: x(..)
        print *, size(x)
        print *, rank(x)
    end subroutine
end program