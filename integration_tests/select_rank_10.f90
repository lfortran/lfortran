program select_rank_10
    implicit none

    integer :: a(3) = [1, 2, 3]
    real    :: b(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2,2])

    call check(a)
    call check(b)

contains

    subroutine check(x)
        class(*), intent(in) :: x(..)

        select rank (x)
        rank (1)
            print *, "Rank-1 array"
            select type (x)
            type is (integer)
                print *, "Integer"
                print *, x
            type is (real)
                error stop
            end select

        rank (2)
            print *, "Rank-2 array"
            select type (x)
            type is (integer)
                error stop
            type is (real)
                print *, "Real"
                print *, x
            end select
        end select

    end subroutine check

end program select_rank_10
