! A procedure declares the block of common_54_mod with other names, before
! the program that uses the module in the same file.
subroutine common_54_sum(s)
    implicit none
    integer, intent(out) :: s
    integer :: p, q(2)
    common /common_54_blk/ p, q
    s = p + 10 * q(1) + 100 * q(2)
end subroutine common_54_sum

program common_54
    use common_54_mod, only: a, c, bind
    implicit none
    external :: common_54_legacy
    integer, pointer :: x
    integer :: s
    call bind(x)
    call common_54_legacy(17)
    print *, a, c
    if (a /= 17) error stop
    if (x /= 17) error stop
    if (any(c /= [18, 19])) error stop
    call common_54_sum(s)
    if (s /= 2097) error stop
    c(2) = 0
    call common_54_sum(s)
    if (s /= 197) error stop
end program common_54
