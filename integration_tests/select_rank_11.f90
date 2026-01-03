program select_rank_11
    implicit none
    call check_scalar(5, 0)
    
    print *, "All tests passed"

contains

    subroutine check_scalar(x, expected)
        integer, intent(in) :: x(..)
        integer, intent(in) :: expected

        select rank(x)
            rank(0)
                if (expected /= 0) error stop "Expected rank 0 for scalar"
            rank default
                error stop "Unexpected rank for scalar input"
        end select
    end subroutine check_scalar

end program select_rank_11
