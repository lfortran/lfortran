module dtio_write_10_user
use dtio_write_10_ma, only: ta => t
use dtio_write_10_mb, only: tb => t
implicit none
contains
subroutine show_a(x, s)
    type(ta), intent(in) :: x
    character(*), intent(out) :: s
    write(s, '(dt)') x
end subroutine
subroutine show_b(y, s)
    type(tb), intent(in) :: y
    character(*), intent(out) :: s
    write(s, '(dt)') y
end subroutine
end module
