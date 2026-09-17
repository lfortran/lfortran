module assumed_rank_20_mod
contains
    subroutine read_assumed_shape(u, x)
        integer, intent(in) :: u
        real(8), intent(inout) :: x(:)

        read(u, '(3F6.1)') x
    end subroutine

    subroutine read_assumed_rank(u, x)
        integer, intent(in) :: u
        real(8), intent(inout) :: x(..)

        select rank (x)
        rank (1)
            read(u, '(3F6.1)') x
        rank default
            error stop
        end select
    end subroutine
end module

program assumed_rank_20
    use assumed_rank_20_mod
    implicit none

    real(8) :: a(3), b(3)

    a = 0.0d0
    call fill_file_and_read_shape(a)
    call check_values(a, 1.0d0, 2.0d0, 3.0d0)

    b = 0.0d0
    call fill_file_and_read_rank(b)
    call check_values(b, 4.0d0, 5.0d0, 6.0d0)

contains
    subroutine fill_file_and_read_shape(x)
        real(8), intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_shape.tmp', status='replace', &
             action='readwrite')
        write(u, '(3F6.1)') 1.0d0, 2.0d0, 3.0d0
        rewind(u)
        call read_assumed_shape(u, x)
        close(u, status='delete')
    end subroutine

    subroutine fill_file_and_read_rank(x)
        real(8), intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_rank.tmp', status='replace', &
             action='readwrite')
        write(u, '(3F6.1)') 4.0d0, 5.0d0, 6.0d0
        rewind(u)
        call read_assumed_rank(u, x)
        close(u, status='delete')
    end subroutine

    subroutine check_values(x, e1, e2, e3)
        real(8), intent(in) :: x(:)
        real(8), intent(in) :: e1, e2, e3

        if (size(x) /= 3) error stop
        if (x(1) /= e1) error stop
        if (x(2) /= e2) error stop
        if (x(3) /= e3) error stop
    end subroutine
end program
