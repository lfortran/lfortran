program select_rank_26
    implicit none
    real :: x(3)
    x = [1.0, 2.0, 3.0]
    call foo(x)
    print *, "ok"
contains
    subroutine foo(input)
        class(*), dimension(..), intent(in) :: input
        rank_select: select rank(input)
        rank(1)
            select type(input)
            type is(real)
                exit rank_select
            end select
            error stop
        end select rank_select
    end subroutine
end program
