module select_rank_25_mod
    implicit none
contains
    subroutine foo(input)
        class(*), dimension(..), intent(in) :: input
        select rank(input)
        rank(2)
            select type(input)
            class default
                if (size(input) /= 12) error stop
            end select
        end select
    end subroutine
end module

program select_rank_25
    use select_rank_25_mod
    real :: arr(3, 4)
    arr = 1.0
    call foo(arr)
    print *, "ok"
end program
