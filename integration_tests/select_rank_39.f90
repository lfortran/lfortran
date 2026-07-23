program select_rank_39
    implicit none

    character :: c(1)

    c(1) = "A"
    call check(c)

contains

    subroutine check(a)
        character, intent(in) :: a(..)

        select rank(a)
        rank(1)
            if (rank(a) /= 1) error stop
            if (a(1) /= "A") error stop
        rank default
            error stop
        end select
    end subroutine

end program
