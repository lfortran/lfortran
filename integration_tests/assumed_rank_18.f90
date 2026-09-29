module assumed_rank_18_mod
    implicit none
contains

    subroutine check_real(var)
        real(8), intent(in) :: var(..)
        integer :: u
        character(128) :: line

        select rank (var)
        rank (0)
            open(newunit=u, file="assumed_rank_18.out", status="replace", action="readwrite")
            write(u, *) var
            rewind(u)
            read(u, '(a)') line
            close(u, status="delete")
            if (index(line, "4.000") == 0) error stop "real rank 0 write missing value"
        rank (1)
            open(newunit=u, file="assumed_rank_18.out", status="replace", action="readwrite")
            write(u, *) var
            rewind(u)
            read(u, '(a)') line
            close(u, status="delete")
            if (index(line, "1.000") == 0) error stop "real rank 1 write missing first value"
            if (index(line, "3.000") == 0) error stop "real rank 1 write missing last value"
        rank default
            error stop "unexpected real rank"
        end select
    end subroutine check_real

    subroutine check_integer(var)
        integer, intent(in) :: var(..)
        integer :: u
        character(128) :: line

        select rank (var)
        rank (0)
            open(newunit=u, file="assumed_rank_18.out", status="replace", action="readwrite")
            write(u, *) var
            rewind(u)
            read(u, '(a)') line
            close(u, status="delete")
            if (index(line, "40") == 0) error stop "integer rank 0 write missing value"
        rank (1)
            open(newunit=u, file="assumed_rank_18.out", status="replace", action="readwrite")
            write(u, *) var
            rewind(u)
            read(u, '(a)') line
            close(u, status="delete")
            if (index(line, "10") == 0) error stop "integer rank 1 write missing first value"
            if (index(line, "30") == 0) error stop "integer rank 1 write missing last value"
        rank default
            error stop "unexpected integer rank"
        end select
    end subroutine check_integer

end module assumed_rank_18_mod

program assumed_rank_18
    use assumed_rank_18_mod
    implicit none

    real(8) :: rs
    real(8) :: ra(3)
    integer :: is
    integer :: ia(3)

    rs = 4.0d0
    ra = [1.0d0, 2.0d0, 3.0d0]
    is = 40
    ia = [10, 20, 30]

    call check_real(rs)
    call check_real(ra)
    call check_integer(is)
    call check_integer(ia)

end program assumed_rank_18
