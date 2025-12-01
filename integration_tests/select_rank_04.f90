program select_rank_04
    implicit none

    integer :: a(2) = [4, 2]

    call check(a)

contains

    subroutine check(x)
        integer, intent(in) :: x(..)
        integer :: l1(1)
        integer :: l2(2)
    
        select rank(x)
        rank (1)
            print *, x
        rank (2)
            print *, x
        end select
    end subroutine check

end program select_rank_04