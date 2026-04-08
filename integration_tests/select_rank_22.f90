program select_rank_22
    implicit none
    real :: val
    val = 3.14
    call test_sub(val)
    if (abs(val - 42.0) > 1.0e-5) error stop

contains

    subroutine test_sub(x)
        class(*), dimension(..), intent(inout) :: x
        select rank(x)
        rank(0)
            select type(x)
            type is (real)
                x = 42.0
            end select
        rank(1)
            select type(x)
            type is (real)
                x(1) = 42.0
            end select
        end select
    end subroutine

end program select_rank_22
