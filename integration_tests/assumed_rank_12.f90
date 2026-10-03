program assumed_rank_12
    ! Associating an assumed-size actual argument with an assumed-rank
    ! dummy argument is valid Fortran 2018 (see issue #11851).
    implicit none
    integer :: a(4)
    a = [1, 2, 3, 4]
    call forward(a)
contains
    subroutine forward(items)
        integer :: items(*)
        call consume(items)
        if (consume_fn(items) /= 1) error stop
    end subroutine
    subroutine consume(x)
        integer :: x(..)
        if (rank(x) /= 1) error stop
    end subroutine
    integer function consume_fn(x)
        integer :: x(..)
        consume_fn = rank(x)
    end function
end program
